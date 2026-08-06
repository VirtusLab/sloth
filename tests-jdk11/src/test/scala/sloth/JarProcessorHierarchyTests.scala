package sloth

import munit.FunSuite
import org.objectweb.asm.tree.{ClassNode, FrameNode}
import org.objectweb.asm.{ClassReader, Opcodes}
import sloth.jar.JarProcessor
import sloth.patching.{ClassHierarchyException, ClassHierarchySource, ClassLoaderClassWriter}

import java.io.InputStream
import java.net.{URL, URLClassLoader}
import java.nio.file.{Files, Path, Paths}
import java.util.jar.{JarEntry, JarOutputStream}
import scala.jdk.CollectionConverters.*

/** Regression suite for the class-hierarchy resolution used while recomputing stack map frames.
  *
  * Patching rewrites methods and lets ASM recompute frames, which requires merging reference types. When a merge
  * involves types the resolver cannot see, ASM widens the merge to `java/lang/Object`, the frames no longer describe
  * the code, and the JVM rejects the class with a `VerifyError`. See https://github.com/VirtusLab/sloth/issues/1.
  *
  * The `cross-jar-hierarchy` fixture is compiled as one unit and then split into two jars, so the jar being patched
  * genuinely cannot see the base/subclass hierarchy its methods merge.
  */
class JarProcessorHierarchyTests extends FunSuite with ExampleLoader {

  override val munitTimeout = scala.concurrent.duration.Duration(600, "s")

  override val examplesDir: os.Path = os.pwd / "tests" / "src" / "test" / "resources" / "fixtures" / "examples"
  override val testWorkspace: os.Path = os.temp.dir(prefix = "sloth-jar-hierarchy-tests-", deleteOnExit = false)
  override val quietCompilation: Boolean = true
  override val quietTests: Boolean = true

  /** The OFFSET-based range, where patching actually rewrites bodies and recomputes frames. */
  val hierarchyVersions: Seq[String] = Seq("3.3.0", "3.7.3")

  override def requiredScalaVersions: Seq[String] = hierarchyVersions

  val exampleName = "cross-jar-hierarchy"
  val holderInternalName = "slothtest/crossjar/CrossJarHolder$"
  val baseInternalName = "slothtest/crossjar/HierBase"
  val hierarchyClassNames: Set[String] =
    Set("slothtest/crossjar/HierBase", "slothtest/crossjar/HierSubA", "slothtest/crossjar/HierSubB")

  override def beforeAll(): Unit =
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(minimumLevel = Some(scribe.Level.Error))
      .replace()

  lazy val example: LoadedExample =
    loadExample(exampleName).getOrElse(fail(s"Failed to compile the '$exampleName' fixture"))

  /** Directory holding the compiled classfiles of the fixture for one Scala version. */
  def compiledDir(version: String): os.Path =
    val versionResult = example.compilationResult.results
      .get(version)
      .filter(_.success)
      .getOrElse(fail(s"Fixture '$exampleName' did not compile with Scala $version"))
    val anyFile = versionResult.classFiles.head
    os.Path(anyFile.absolutePath.toString.stripSuffix(anyFile.relativePath).stripSuffix("/"))

  /** scala-library (and friends) for the fixture, needed to actually initialize the patched class. */
  def scalaRuntimeUrls(version: String): Seq[URL] =
    val targetDir = testWorkspace / exampleName / version
    val cp = os
      .proc(
        "scala-cli",
        "compile",
        "--print-classpath",
        "--jvm",
        "17",
        "--release",
        "9",
        "--bloop-startup-timeout",
        "180s",
        "-S",
        version,
        targetDir.toString
      )
      .call(cwd = targetDir, stderr = os.Pipe, stdout = os.Pipe)
      .out
      .text()
      .trim
    cp.split(java.io.File.pathSeparatorChar).filter(_.nonEmpty).map(p => Paths.get(p).toUri.toURL).toSeq

  /** Write the given classfiles (internal name -> bytes) into a jar. */
  def writeJar(target: os.Path, classes: Map[String, Array[Byte]]): Path =
    val jos = new JarOutputStream(Files.newOutputStream(target.toNIO))
    try
      classes.foreach { (internalName, bytes) =>
        jos.putNextEntry(new JarEntry(internalName + ".class"))
        jos.write(bytes)
        jos.closeEntry()
      }
    finally jos.close()
    target.toNIO

  /** Split the compiled fixture into `lib-a.jar` (the class to patch) and `lib-b.jar` (hierarchy). */
  def splitFixtureJars(version: String, label: String): (Path, Path) =
    val dir = compiledDir(version)
    val all = os
      .walk(dir)
      .filter(p => os.isFile(p) && p.last.endsWith(".class"))
      .map(p => p.relativeTo(dir).toString.stripSuffix(".class") -> os.read.bytes(p))
      .toMap
    val (hierarchy, rest) = all.partition((name, _) => hierarchyClassNames.contains(name))
    assertEquals(
      hierarchy.keySet,
      hierarchyClassNames,
      s"Fixture should provide the full hierarchy; found ${all.keySet}"
    )
    val jarDir = testWorkspace / "jars" / version / label
    os.makeDir.all(jarDir)
    (writeJar(jarDir / "lib-a.jar", rest), writeJar(jarDir / "lib-b.jar", hierarchy))

  def readJarEntry(jar: Path, internalName: String): Array[Byte] =
    val zip = new java.util.zip.ZipFile(jar.toFile)
    try
      val entry = Option(zip.getEntry(internalName + ".class"))
        .getOrElse(fail(s"$internalName.class missing from $jar"))
      zip.getInputStream(entry).readAllBytes()
    finally zip.close()

  /** All reference types mentioned by the stack map frames of the methods with the given prefix. */
  def frameTypes(classBytes: Array[Byte], methodNamePrefix: String): Set[String] =
    val node = new ClassNode(Opcodes.ASM9)
    new ClassReader(classBytes).accept(node, 0)
    val methods = node.methods.asScala.filter(_.name.startsWith(methodNamePrefix))
    assert(methods.nonEmpty, s"No method starting with '$methodNamePrefix' in ${node.name}")
    methods.flatMap { m =>
      m.instructions
        .iterator()
        .asScala
        .collect { case f: FrameNode =>
          val locals = Option(f.local).map(_.asScala.toSeq).getOrElse(Seq.empty)
          val stack = Option(f.stack).map(_.asScala.toSeq).getOrElse(Seq.empty)
          (locals ++ stack).collect { case s: String => s }
        }
        .flatten
    }.toSet

  for version <- hierarchyVersions if isExampleSelected(exampleName) && isScalaVersionSelected(version) do

    test(s"hierarchy classpath keeps stack map frames precise (Scala $version)") {
      val (libA, libB) = splitFixtureJars(version, "frames")
      val patched = (testWorkspace / "jars" / version / "frames" / "lib-a-patched.jar").toNIO

      val result = JarProcessor.process(libA, patched, hierarchyClasspath = Seq(libB))
      assertEquals(result.errors, Seq.empty[String], "Patching should not report errors")
      assert(result.patchedClasses > 0, "The holder class should have been patched")

      val holderBytes = readJarEntry(patched, holderInternalName)
      Seq("choose", "picked$lzyINIT").foreach { method =>
        val types = frameTypes(holderBytes, method)
        assert(
          types.contains(baseInternalName),
          s"$method frames should merge the subclasses to $baseInternalName, got $types"
        )
      }
    }

    test(s"patched class loads and initializes over the hierarchy jar (Scala $version)") {
      val (libA, libB) = splitFixtureJars(version, "runtime")
      val patched = (testWorkspace / "jars" / version / "runtime" / "lib-a-patched.jar").toNIO

      JarProcessor.process(libA, patched, hierarchyClasspath = Seq(libB))

      val urls = Seq(patched.toUri.toURL, libB.toUri.toURL) ++ scalaRuntimeUrls(version)
      val loader = new URLClassLoader(urls.toArray, ClassLoader.getPlatformClassLoader)
      try
        val holder = Class.forName("slothtest.crossjar.CrossJarHolder$", true, loader)
        val module = holder.getField("MODULE$").get(null)
        assertEquals(holder.getMethod("picked").invoke(module), "A")
        val chosen = holder.getMethod("choose").invoke(module)
        assertEquals(chosen.getClass.getMethod("label").invoke(chosen), "A")
      finally loader.close()
    }

    test(s"missing hierarchy context is reported instead of silently widening (Scala $version)") {
      val (libA, _) = splitFixtureJars(version, "strict")
      val patched = (testWorkspace / "jars" / version / "strict" / "lib-a-patched.jar").toNIO

      val result = JarProcessor.process(libA, patched, strictHierarchy = true)
      assert(result.errors.nonEmpty, "An unresolvable hierarchy must be reported, not silently widened")
      assert(
        result.errors.exists(_.contains("slothtest/crossjar/HierSub")),
        s"The report should name the unresolvable type, got ${result.errors}"
      )
    }

  /** A loader that can serve no `.class` resource at all, like a GraalVM native image. */
  class ResourceLessClassLoader extends ClassLoader(null) {
    override def getResourceAsStream(name: String): InputStream = null
  }

  test("JDK types merge precisely even when no .class resource is readable") {
    val blind = ClassHierarchySource.forClassLoader(new ResourceLessClassLoader)
    assertEquals(blind.classBytes("java/util/zip/ZipFile"), None, "Precondition: no .class resources")

    val writer = new ClassLoaderClassWriter(ClassHierarchySource.chain(blind, ClassHierarchySource.jdkStubs))
    assertEquals(writer.getCommonSuperClass("java/util/jar/JarFile", "java/util/zip/ZipFile"), "java/util/zip/ZipFile")
    assertEquals(writer.getCommonSuperClass("java/lang/Integer", "java/lang/Long"), "java/lang/Number")
  }

  test("unresolvable types raise instead of degrading to Object in strict mode") {
    val strict = ClassHierarchySource.strict(ClassHierarchySource.chain())
    val writer = new ClassLoaderClassWriter(strict)
    val failure = intercept[ClassHierarchyException] {
      writer.getCommonSuperClass("some/unknown/SubA", "some/unknown/SubB")
    }
    assertEquals(failure.internalName, "some/unknown/SubA")

    val lenient = new ClassLoaderClassWriter(ClassHierarchySource.chain())
    assertEquals(
      lenient.getCommonSuperClass("some/unknown/SubA", "some/unknown/SubB"),
      "java/lang/Object",
      "Non-strict resolution still degrades, but logs a warning"
    )
  }

  test("Object stays the answer when it genuinely is the common supertype") {
    val writer = new ClassLoaderClassWriter(ClassHierarchySource.jdkStubs)
    assertEquals(writer.getCommonSuperClass("java/lang/Integer", "java/lang/Thread"), "java/lang/Object")
  }
}
