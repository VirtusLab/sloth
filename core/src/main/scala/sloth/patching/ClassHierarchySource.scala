package sloth.patching

import org.objectweb.asm.{ClassWriter, Opcodes}

import java.io.Closeable
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile
import scala.collection.mutable

/** Supplies the classfile bytes ASM needs to answer "what is the common supertype of these two types?" while
  * recomputing stack map frames.
  *
  * Answers are bytes rather than `Class` objects on purpose: in agent mode loading a class early would hand the JVM
  * unpatched bytes, and inside a GraalVM native image there are no `.class` resources to read and `Class.forName`
  * against a caller-supplied loader does not work either.
  */
trait ClassHierarchySource extends Closeable:

  /** Classfile bytes for a JVM internal name (`a/b/C`), or `None` if this source cannot serve it. */
  def classBytes(internalName: String): Option[Array[Byte]]

  /** When true, an unresolvable type aborts patching instead of widening the merge to `Object`. */
  def failOnUnresolved: Boolean = false

  def close(): Unit = ()

object ClassHierarchySource:

  /** Prefixes whose types are guaranteed free of Scala lazy vals, so reflecting on them is safe. */
  private val jdkPrefixes = Array("java/", "javax/", "jdk/", "sun/", "com/sun/")

  /** The class-level subset of the access flags; `Class.getModifiers` also reports member flags. */
  private val classAccessMask =
    Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL | Opcodes.ACC_INTERFACE |
      Opcodes.ACC_ABSTRACT | Opcodes.ACC_ANNOTATION | Opcodes.ACC_ENUM

  /** An in-memory source, keyed by internal name. Used for the classfiles already being patched. */
  def fromClassBytes(classes: Map[String, Array[Byte]]): ClassHierarchySource =
    new ClassHierarchySource:
      def classBytes(internalName: String): Option[Array[Byte]] = classes.get(internalName)

  /** Reads `.class` entries directly out of jars and class directories, in the given order. */
  def forClasspath(entries: Seq[Path]): ClassHierarchySource = new ClasspathSource(entries)

  /** Reads `.class` resources from a classloader without ever asking it to load a class. */
  def forClassLoader(loader: ClassLoader): ClassHierarchySource =
    new ClassHierarchySource:
      def classBytes(internalName: String): Option[Array[Byte]] =
        val resource = loader.getResourceAsStream(internalName + ".class")
        if resource == null then None
        else
          try Some(resource.readAllBytes())
          catch case _: Throwable => None
          finally resource.close()

  /** The resolution chain to use when all a caller has is a classloader.
    *
    * The system loader is consulted as well because a restricted loader (an agent's, or one built over a single jar)
    * frequently cannot see the application's own types.
    */
  def forRuntimeClassLoader(loader: ClassLoader): ClassHierarchySource =
    val systemLoader = ClassLoader.getSystemClassLoader
    val loaders =
      if systemLoader ne loader then Seq(forClassLoader(loader), forClassLoader(systemLoader))
      else Seq(forClassLoader(loader))
    chain(loaders :+ jdkStubs*)

  /** Synthesizes supertype-only stubs for JDK types.
    *
    * From Java 9 on, platform types have no readable `.class` resource, and in a GraalVM native image they frequently
    * have no `Class` object either unless the type was registered for reflection — dynamic
    * `Class.forName("java.util.jar.JarFile")` fails even though the class is part of the JDK. A stub carrying just the
    * access flags, superclass and interfaces is everything frame computation needs. Resolution order:
    *   1. class literals for types known to appear in scala3-compiler merges (embeds them in native images) 2.
    *      `Class.forName` via the bootstrap / platform loader 3. hardcoded hierarchy for the JarFile / ZipFile /
    *      JarOutputStream cluster
    */
  val jdkStubs: ClassHierarchySource =
    new ClassHierarchySource:
      private val stubCache = new java.util.concurrent.ConcurrentHashMap[String, Array[Byte]]()

      def classBytes(internalName: String): Option[Array[Byte]] =
        if !jdkPrefixes.exists(internalName.startsWith) then None
        else
          Option(stubCache.get(internalName)).orElse {
            val bytesOpt =
              resolveJdkClass(internalName.replace('/', '.'))
                .map(classStubBytes)
                .orElse(hardcodedJdkStub(internalName))
            bytesOpt.foreach(stubCache.put(internalName, _))
            bytesOpt
          }

      private def resolveJdkClass(binaryName: String): Option[Class[?]] =
        // Prefer class literals for types the native image must see; Class.forName of dynamic
        // names needs reflection metadata under GraalVM and otherwise returns ClassNotFoundException.
        binaryName match
          case "java.util.jar.JarFile"              => Some(classOf[java.util.jar.JarFile])
          case "java.util.jar.JarOutputStream"      => Some(classOf[java.util.jar.JarOutputStream])
          case "java.util.zip.ZipFile"              => Some(classOf[java.util.zip.ZipFile])
          case "java.util.zip.ZipOutputStream"      => Some(classOf[java.util.zip.ZipOutputStream])
          case "java.util.zip.DeflaterOutputStream" => Some(classOf[java.util.zip.DeflaterOutputStream])
          case "java.util.zip.ZipEntry"             => Some(classOf[java.util.zip.ZipEntry])
          case "java.io.FilterOutputStream"         => Some(classOf[java.io.FilterOutputStream])
          case "java.io.OutputStream"               => Some(classOf[java.io.OutputStream])
          case "java.io.IOException"                => Some(classOf[java.io.IOException])
          case "java.io.Closeable"                  => Some(classOf[java.io.Closeable])
          case "java.lang.AutoCloseable"            => Some(classOf[java.lang.AutoCloseable])
          case "java.lang.Exception"                => Some(classOf[java.lang.Exception])
          case "java.lang.Throwable"                => Some(classOf[java.lang.Throwable])
          case "java.lang.Object"                   => Some(classOf[java.lang.Object])
          case "java.io.Serializable"               => Some(classOf[java.io.Serializable])
          case "java.io.File"                       => Some(classOf[java.io.File])
          case "java.lang.String"                   => Some(classOf[java.lang.String])
          case "java.lang.Class"                    => Some(classOf[Class[?]])
          case "java.lang.Error"                    => Some(classOf[java.lang.Error])
          case "java.lang.RuntimeException"         => Some(classOf[java.lang.RuntimeException])
          case "java.lang.ReflectiveOperationException" =>
            Some(classOf[java.lang.ReflectiveOperationException])
          case _ =>
            try Some(Class.forName(binaryName, false, null))
            catch
              case _: Throwable =>
                try Some(Class.forName(binaryName, false, ClassLoader.getPlatformClassLoader))
                catch case _: Throwable => None

      private def classStubBytes(cls: Class[?]): Array[Byte] =
        val internalName = cls.getName.replace('.', '/')
        val superClass = cls.getSuperclass
        val superName =
          if superClass != null then superClass.getName.replace('.', '/')
          else if internalName == "java/lang/Object" then null
          else "java/lang/Object"
        val writer = new ClassWriter(0)
        writer.visit(
          Opcodes.V1_8,
          cls.getModifiers & classAccessMask,
          internalName,
          null,
          superName,
          cls.getInterfaces.map(_.getName.replace('.', '/'))
        )
        writer.visitEnd()
        writer.toByteArray

      /** Last-resort stubs when neither class literals nor Class.forName work. Enough for the known
        * `FileZipArchive.openZipFile` JarFile/ZipFile merge in scala3-compiler.
        */
      private def hardcodedJdkStub(internalName: String): Option[Array[Byte]] =
        internalName match
          case "java/util/jar/JarFile" =>
            Some(manualStub("java/util/jar/JarFile", "java/util/zip/ZipFile", Array.empty, isInterface = false))
          case "java/util/jar/JarOutputStream" =>
            Some(
              manualStub(
                "java/util/jar/JarOutputStream",
                "java/util/zip/ZipOutputStream",
                Array.empty,
                isInterface = false
              )
            )
          case "java/util/zip/ZipOutputStream" =>
            Some(
              manualStub(
                "java/util/zip/ZipOutputStream",
                "java/util/zip/DeflaterOutputStream",
                Array.empty,
                isInterface = false
              )
            )
          case "java/util/zip/DeflaterOutputStream" =>
            Some(
              manualStub(
                "java/util/zip/DeflaterOutputStream",
                "java/io/FilterOutputStream",
                Array.empty,
                isInterface = false
              )
            )
          case "java/io/FilterOutputStream" =>
            Some(
              manualStub(
                "java/io/FilterOutputStream",
                "java/io/OutputStream",
                Array.empty,
                isInterface = false
              )
            )
          case "java/io/OutputStream" =>
            Some(
              manualStub(
                "java/io/OutputStream",
                "java/lang/Object",
                Array("java/io/Closeable", "java/lang/AutoCloseable"),
                isInterface = false
              )
            )
          case "java/util/zip/ZipFile" =>
            Some(
              manualStub(
                "java/util/zip/ZipFile",
                "java/lang/Object",
                Array("java/io/Closeable", "java/lang/AutoCloseable"),
                isInterface = false
              )
            )
          case "java/io/Closeable" =>
            Some(
              manualStub(
                "java/io/Closeable",
                "java/lang/Object",
                Array("java/lang/AutoCloseable"),
                isInterface = true
              )
            )
          case "java/lang/AutoCloseable" =>
            Some(manualStub("java/lang/AutoCloseable", "java/lang/Object", Array.empty, isInterface = true))
          case "java/io/IOException" =>
            Some(manualStub("java/io/IOException", "java/lang/Exception", Array.empty, isInterface = false))
          case "java/lang/Exception" =>
            Some(manualStub("java/lang/Exception", "java/lang/Throwable", Array.empty, isInterface = false))
          case "java/lang/Throwable" =>
            Some(
              manualStub(
                "java/lang/Throwable",
                "java/lang/Object",
                Array("java/io/Serializable"),
                isInterface = false
              )
            )
          case "java/io/Serializable" =>
            Some(manualStub("java/io/Serializable", "java/lang/Object", Array.empty, isInterface = true))
          case "java/lang/Object" =>
            Some(manualStub("java/lang/Object", null, Array.empty, isInterface = false))
          case _ => None

      private def manualStub(
          internalName: String,
          superName: String,
          interfaces: Array[String],
          isInterface: Boolean
      ): Array[Byte] =
        val access =
          Opcodes.ACC_PUBLIC |
            (if isInterface then Opcodes.ACC_INTERFACE | Opcodes.ACC_ABSTRACT else 0)
        val writer = new ClassWriter(0)
        writer.visit(Opcodes.V1_8, access, internalName, null, superName, interfaces)
        writer.visitEnd()
        writer.toByteArray

  /** Tries each source in order and returns the first hit. */
  def chain(sources: ClassHierarchySource*): ClassHierarchySource =
    new ClassHierarchySource:
      def classBytes(internalName: String): Option[Array[Byte]] =
        sources.iterator.map(_.classBytes(internalName)).collectFirst { case Some(bytes) => bytes }
      override def failOnUnresolved: Boolean = sources.exists(_.failOnUnresolved)
      override def close(): Unit = sources.foreach(_.close())

  /** Same resolution, but an unresolvable type raises [[ClassHierarchyException]]. */
  def strict(source: ClassHierarchySource): ClassHierarchySource =
    new ClassHierarchySource:
      def classBytes(internalName: String): Option[Array[Byte]] = source.classBytes(internalName)
      override def failOnUnresolved: Boolean = true
      override def close(): Unit = source.close()

  /** Serves classfiles from jars (via `ZipFile`) and class directories (via plain file reads). */
  private final class ClasspathSource(entries: Seq[Path]) extends ClassHierarchySource:

    private val zipFiles = mutable.LinkedHashMap.empty[Path, Option[ZipFile]]

    // Synchronized because a single source is shared across the writers of every patched group.
    private def zipFileFor(entry: Path): Option[ZipFile] = zipFiles.synchronized {
      zipFiles.getOrElseUpdate(
        entry,
        try Some(new ZipFile(entry.toFile))
        catch case _: Throwable => None
      )
    }

    private def fromJar(entry: Path, resource: String): Option[Array[Byte]] =
      zipFileFor(entry).flatMap { zip =>
        Option(zip.getEntry(resource)).flatMap { zipEntry =>
          val in = zip.getInputStream(zipEntry)
          try Some(in.readAllBytes())
          catch case _: Throwable => None
          finally in.close()
        }
      }

    private def fromDirectory(entry: Path, resource: String): Option[Array[Byte]] =
      val file = entry.resolve(resource)
      if !Files.isRegularFile(file) then None
      else
        try Some(Files.readAllBytes(file))
        catch case _: Throwable => None

    def classBytes(internalName: String): Option[Array[Byte]] =
      val resource = internalName + ".class"
      entries.iterator
        .map(entry => if Files.isDirectory(entry) then fromDirectory(entry, resource) else fromJar(entry, resource))
        .collectFirst { case Some(bytes) => bytes }

    override def close(): Unit = zipFiles.synchronized {
      zipFiles.values.flatten.foreach { zip =>
        try zip.close()
        catch case _: Throwable => ()
      }
      zipFiles.clear()
    }
