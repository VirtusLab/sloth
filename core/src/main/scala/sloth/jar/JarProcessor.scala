package sloth.jar

import sloth.analysis.LazyValAnalyzer
import sloth.patching.{BytecodePatcher, ClassHierarchySource}

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.file.Path
import java.util.jar.{JarEntry, JarInputStream, JarOutputStream, Manifest}
import scala.collection.mutable

object JarProcessor:

  case class JarResult(
      totalClasses: Int,
      patchedClasses: Int,
      failedClasses: Int,
      errors: Seq[String]
  )

  /** Process a JAR: patch all .class entries, write to output path. Non-class entries pass through unchanged.
    *
    * Patching recomputes stack map frames, which requires resolving the supertypes of every reference type a rewritten
    * method merges. Types defined outside the input JAR are only resolvable if the caller says where they live, so
    * downstream tools should pass the rest of the application's classpath as `hierarchyClasspath` (jars and/or class
    * directories). Without it an unresolvable merge widens to `java/lang/Object` and the patched class fails
    * verification at load time — see https://github.com/VirtusLab/sloth/issues/1.
    *
    * @param hierarchyClasspath
    *   Jars and class directories defining types the input JAR references. The input JAR itself is always consulted
    *   first, so its own classes win.
    * @param strictHierarchy
    *   Fail the group with a diagnostic instead of emitting frames widened to `java/lang/Object`.
    */
  def process(
      inputJar: Path,
      outputJar: Path,
      hierarchyClasspath: Seq[Path] = Nil,
      strictHierarchy: Boolean = false
  ): JarResult =
    val classEntries = mutable.LinkedHashMap[String, Array[Byte]]()
    val nonClassEntries = mutable.LinkedHashMap[String, Array[Byte]]()
    var manifest: Option[Manifest] = None

    // Read all entries from input JAR
    val jis = new JarInputStream(java.nio.file.Files.newInputStream(inputJar))
    try
      manifest = Option(jis.getManifest)
      var entry = jis.getNextJarEntry
      while entry != null do
        val name = entry.getName
        if !entry.isDirectory then
          val bytes = readAllBytes(jis)
          if name.endsWith(".class") then classEntries(name) = bytes
          else nonClassEntries(name) = bytes
        entry = jis.getNextJarEntry
    finally jis.close()

    // Convert entry paths to dotted class names for the analyzer
    val classfileMap: Map[String, Array[Byte]] = classEntries.map { (entryPath, bytes) =>
      val className = entryPath.stripSuffix(".class").replace('/', '.')
      className -> bytes
    }.toMap

    // Build reverse mapping: dotted name -> entry path
    val nameToEntryPath: Map[String, String] = classEntries.keys.map { entryPath =>
      val className = entryPath.stripSuffix(".class").replace('/', '.')
      className -> entryPath
    }.toMap

    // Hierarchy context for frame computation: the JAR's own (in-memory) classes first, then the
    // caller-supplied classpath, then the host classpath and synthesized JDK stubs as fallbacks.
    val baseHierarchy = ClassHierarchySource.chain(
      ClassHierarchySource.fromClassBytes(classEntries.map((path, bytes) => path.stripSuffix(".class") -> bytes).toMap),
      ClassHierarchySource.forClasspath(hierarchyClasspath),
      ClassHierarchySource.forClassLoader(getClass.getClassLoader),
      ClassHierarchySource.jdkStubs
    )
    val hierarchy = if strictHierarchy then ClassHierarchySource.strict(baseHierarchy) else baseHierarchy

    // Group and patch
    val errors = mutable.ArrayBuffer[String]()
    val patchedBytes = mutable.Map[String, Array[Byte]]() // keyed by entry path

    LazyValAnalyzer.group(classfileMap) match
      case Left(error) =>
        errors += s"Failed to group classfiles: $error"

      case Right(groups) =>
        for group <- groups do
          try
            BytecodePatcher.patch(group, hierarchySource = Some(hierarchy)) match
              case BytecodePatcher.PatchResult.PatchedSingle(name, bytes) =>
                nameToEntryPath.get(name).foreach(ep => patchedBytes(ep) = bytes)

              case BytecodePatcher.PatchResult.PatchedPair(objName, clsName, objBytes, clsBytes) =>
                nameToEntryPath.get(objName).foreach(ep => patchedBytes(ep) = objBytes)
                nameToEntryPath.get(clsName).foreach(ep => patchedBytes(ep) = clsBytes)

              case BytecodePatcher.PatchResult.NotApplicable => // nothing to do
              case BytecodePatcher.PatchResult.Failed(error) =>
                errors += s"Failed to patch group ${group.primaryName}: $error"
          catch
            case e: Exception =>
              errors += s"Exception patching group ${group.primaryName}: ${e.getMessage}"

    hierarchy.close()

    // Write output JAR
    val jos = manifest match
      case Some(m) => new JarOutputStream(java.nio.file.Files.newOutputStream(outputJar), m)
      case None    => new JarOutputStream(java.nio.file.Files.newOutputStream(outputJar))
    try
      // Write class entries (patched or original)
      for (entryPath, originalBytes) <- classEntries do
        val je = new JarEntry(entryPath)
        jos.putNextEntry(je)
        jos.write(patchedBytes.getOrElse(entryPath, originalBytes))
        jos.closeEntry()

      // Write non-class entries verbatim
      for (entryPath, bytes) <- nonClassEntries do
        val je = new JarEntry(entryPath)
        jos.putNextEntry(je)
        jos.write(bytes)
        jos.closeEntry()
    finally jos.close()

    JarResult(
      totalClasses = classEntries.size,
      patchedClasses = patchedBytes.size,
      failedClasses = errors.size,
      errors = errors.toSeq
    )

  private def readAllBytes(is: InputStream): Array[Byte] =
    val buf = new Array[Byte](8192)
    val out = new ByteArrayOutputStream()
    var n = is.read(buf)
    while n != -1 do
      out.write(buf, 0, n)
      n = is.read(buf)
    out.toByteArray
