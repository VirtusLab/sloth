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
    * From Java 9 on, platform types have no readable `.class` resource, and in a native image they have no classfile at
    * all — but their `Class` objects are always available to Sloth's own loader. A stub carrying just the access flags,
    * superclass and interfaces is everything frame computation needs, and producing bytes keeps the resolution path
    * uniform.
    */
  val jdkStubs: ClassHierarchySource =
    new ClassHierarchySource:
      def classBytes(internalName: String): Option[Array[Byte]] =
        if !jdkPrefixes.exists(internalName.startsWith) then None
        else
          try
            val cls = Class.forName(internalName.replace('/', '.'), false, getClass.getClassLoader)
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
            Some(writer.toByteArray)
          catch case _: Throwable => None

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
