package sloth.patching

import org.objectweb.asm.{ClassReader, ClassWriter, Opcodes}

/** ClassWriter subclass that resolves class hierarchies WITHOUT triggering class loading.
  *
  * ASM's COMPUTE_FRAMES needs to find common superclasses. The default implementation uses Class.forName() which
  * triggers JVM class loading — problematic in agent mode because it can load classes with unpatched bytes before the
  * transformer has a chance to patch them.
  *
  * This implementation reads class bytecode via getResourceAsStream and parses superclass names with ASM's ClassReader,
  * walking the hierarchy without ever calling loadClass().
  *
  * On Java 9+, JDK classes in platform modules (java.base, etc.) may not be accessible via getResourceAsStream from
  * arbitrary classloaders. For JDK types (java/, javax/, jdk/, sun/), we fall back to Class.forName() which is safe —
  * these classes are already loaded or are guaranteed not to contain Scala lazy vals.
  */
class ClassLoaderClassWriter(classLoader: ClassLoader) extends ClassWriter(ClassWriter.COMPUTE_FRAMES):

  private case class ClassInfo(superName: String, interfaces: Array[String], isInterface: Boolean)

  /** JDK package prefixes where Class.forName fallback is safe (no lazy val patching risk). */
  private val jdkPrefixes = Array("java/", "javax/", "jdk/", "sun/", "com/sun/")

  private def readClassInfo(internalName: String): Option[ClassInfo] =
    if internalName == "java/lang/Object" then Some(ClassInfo(null, Array.empty, isInterface = false))
    else
      readClassInfoFromResource(classLoader, internalName)
        .orElse {
          // Try system classloader if different — covers cases where the provided
          // classloader can't see platform/system classes
          val sysCl = ClassLoader.getSystemClassLoader
          if (sysCl ne classLoader) then readClassInfoFromResource(sysCl, internalName) else None
        }
        .orElse(readClassInfoViaReflection(internalName))

  private def readClassInfoFromResource(cl: ClassLoader, internalName: String): Option[ClassInfo] =
    val resource = cl.getResourceAsStream(internalName + ".class")
    if resource == null then None
    else
      try
        val reader = new ClassReader(resource)
        val isInterface = (reader.getAccess & Opcodes.ACC_INTERFACE) != 0
        Some(ClassInfo(reader.getSuperName, reader.getInterfaces, isInterface))
      catch case _: Throwable => None
      finally resource.close()

  /** Fallback: use Class.forName for JDK types that can't be found via getResourceAsStream on Java 9+ (module system).
    * This is safe because JDK classes never contain Scala lazy vals, so loading them won't trigger unwanted patching.
    */
  private def readClassInfoViaReflection(internalName: String): Option[ClassInfo] =
    if !jdkPrefixes.exists(internalName.startsWith) then None
    else
      try
        val cls = Class.forName(internalName.replace('/', '.'), false, classLoader)
        val isInterface = cls.isInterface
        val superName = if cls.getSuperclass != null then cls.getSuperclass.getName.replace('.', '/') else null
        val interfaces = cls.getInterfaces.map(_.getName.replace('.', '/'))
        Some(ClassInfo(superName, interfaces, isInterface))
      catch case _: Throwable => None

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

  private def isAssignableFrom(target: String, source: String): Boolean =
    if target == source then return true
    if target == "java/lang/Object" then return true
    // Walk source's hierarchy checking superclasses and interfaces
    var queue = List(source)
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

    val info1 = readClassInfo(type1)
    val info2 = readClassInfo(type2)

    // If we can't read either class, fall back to Object
    if info1.isEmpty || info2.isEmpty then return "java/lang/Object"

    if isAssignableFrom(type1, type2) then type1
    else if isAssignableFrom(type2, type1) then type2
    else if info1.get.isInterface || info2.get.isInterface then "java/lang/Object"
    else
      // Walk type1's superclass chain until we find one that type2 is assignable to
      val supers1 = getSuperClasses(type1)
      // supers1 is in root-first order, so iterate from the end (most specific first)
      supers1.reverse.find(s => isAssignableFrom(s, type2)).getOrElse("java/lang/Object")
