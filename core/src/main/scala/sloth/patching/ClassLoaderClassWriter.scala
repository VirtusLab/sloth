package sloth.patching

import org.objectweb.asm.{ClassReader, ClassWriter, Opcodes}

import scala.collection.mutable

/** ClassWriter subclass that resolves class hierarchies WITHOUT triggering class loading.
  *
  * ASM's COMPUTE_FRAMES needs to find common superclasses. The default implementation uses Class.forName() which
  * triggers JVM class loading — problematic in agent mode because it can load classes with unpatched bytes before the
  * transformer has a chance to patch them.
  *
  * Supertypes are instead read out of a [[ClassHierarchySource]] with ASM's ClassReader, walking the hierarchy without
  * ever calling loadClass(). A type the source cannot serve is reported (logged, or raised in strict mode) rather than
  * silently widening the merge to `java/lang/Object`, because such a merge yields frames the JVM rejects with a
  * VerifyError.
  */
class ClassLoaderClassWriter(source: ClassHierarchySource) extends ClassWriter(ClassWriter.COMPUTE_FRAMES):

  def this(classLoader: ClassLoader) = this(ClassHierarchySource.forRuntimeClassLoader(classLoader))

  private case class ClassInfo(superName: String, interfaces: Array[String], isInterface: Boolean)

  private val reported = mutable.Set.empty[String]

  private def readClassInfo(internalName: String): Option[ClassInfo] =
    if internalName == "java/lang/Object" then Some(ClassInfo(null, Array.empty, isInterface = false))
    else
      val info = source.classBytes(internalName).flatMap { bytes =>
        try
          val reader = new ClassReader(bytes)
          val isInterface = (reader.getAccess & Opcodes.ACC_INTERFACE) != 0
          Some(ClassInfo(reader.getSuperName, reader.getInterfaces, isInterface))
        catch case _: Throwable => None
      }
      if info.isEmpty then reportUnresolvable(internalName)
      info

  private def reportUnresolvable(internalName: String): Unit =
    if source.failOnUnresolved then throw new ClassHierarchyException(internalName)
    else if reported.add(internalName) then
      scribe.warn(
        s"Cannot resolve class '$internalName' while computing stack map frames; merges involving " +
          "it degrade to java/lang/Object, which can make the patched class fail verification. " +
          "Supply the jars or class directories that define it as hierarchy classpath."
      )

  private def getSuperClasses(internalName: String): List[String] =
    var result = List(internalName)
    var current = internalName
    while current != null && current != "java/lang/Object" do
      readClassInfo(current) match
        case Some(info) if info.superName != null =>
          result = info.superName :: result
          current = info.superName
        case _ =>
          current = null
    result

  private def isAssignableFrom(target: String, subType: String): Boolean =
    if target == subType then return true
    if target == "java/lang/Object" then return true
    // Walk subType's hierarchy checking superclasses and interfaces
    var queue = List(subType)
    var visited = Set.empty[String]
    while queue.nonEmpty do
      val current = queue.head
      queue = queue.tail
      if current == target then return true
      if !visited.contains(current) then
        visited += current
        readClassInfo(current) match
          case Some(info) =>
            if info.superName != null then queue = info.superName :: queue
            queue = info.interfaces.toList ::: queue
          case None => // can't resolve, skip
    false

  override def getCommonSuperClass(type1: String, type2: String): String =
    if type1 == "java/lang/Object" || type2 == "java/lang/Object" then return "java/lang/Object"

    // ASM also merges array types; their element hierarchy is irrelevant to the frames we emit.
    if type1.startsWith("[") || type2.startsWith("[") then return "java/lang/Object"

    val info1 = readClassInfo(type1)
    val info2 = readClassInfo(type2)

    // If we can't read either class, fall back to Object — readClassInfo has already reported it
    if info1.isEmpty || info2.isEmpty then return "java/lang/Object"

    if isAssignableFrom(type1, type2) then type1
    else if isAssignableFrom(type2, type1) then type2
    else if info1.get.isInterface || info2.get.isInterface then "java/lang/Object"
    else
      // Walk type1's superclass chain until we find one that type2 is assignable to
      val supers1 = getSuperClasses(type1)
      // supers1 is in root-first order, so iterate from the end (most specific first)
      supers1.reverse.find(s => isAssignableFrom(s, type2)).getOrElse("java/lang/Object")
