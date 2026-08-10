package sloth.agent

import java.lang.instrument.ClassFileTransformer
import java.security.ProtectionDomain
import java.util.concurrent.ConcurrentHashMap
import org.objectweb.asm.{ClassReader, ClassVisitor, FieldVisitor, Opcodes}
import sloth.analysis.LazyValAnalyzer
import sloth.patching.BytecodePatcher

/** ClassFileTransformer that patches Scala 3.0-3.7 lazy val bytecode at load time.
  *
  * Uses the group-based patching API to correctly handle companion pairs where lazy val implementation is split across
  * object and class files. When one side of a companion pair is loaded, the other side is read via getResourceAsStream
  * (pure I/O, no class loading) and both are patched together. The companion's patched bytes are buffered for when it
  * actually loads.
  */
class SlothTransformer(config: AgentConfig) extends ClassFileTransformer {

  /** Packages to always skip (JDK internals, our own code, Scala runtime). NOTE: String literals for our own package
    * and scala/runtime/ are constructed via StringBuilder/Array to prevent sbt-assembly shade rules from rewriting
    * them. The agent receives UNSHADED class names from the JVM.
    */
  private val skipPrefixes = Array(
    "java/",
    "javax/",
    "jdk/",
    "sun/",
    "com/sun/",
    new StringBuilder("sl").append("oth/").toString,
    new StringBuilder("sca").append("la/runtime/").toString
  )

  /** Buffer for patched companion bytes. When one side of a companion pair is patched, the other side's bytes are
    * stored here and consumed (via remove) when that class loads.
    */
  private val patchedCompanionBuffer = new ConcurrentHashMap[String, Array[Byte]]()

  override def transform(
      loader: ClassLoader,
      className: String,
      classBeingRedefined: Class[?],
      protectionDomain: ProtectionDomain,
      classfileBuffer: Array[Byte]
  ): Array[Byte] = {
    // Fast reject: null className or JDK/internal packages
    if (className == null) return null
    if (skipPrefixes.exists(className.startsWith)) return null

    // Apply include/exclude filters
    if (!config.shouldTransform(className)) return null

    val dotName = className.replace('/', '.')

    scribe.debug(s"transform($dotName): ENTER (${classfileBuffer.length} bytes)")

    // Check if companion already patched this class
    val buffered = patchedCompanionBuffer.remove(dotName)
    if (buffered != null) {
      scribe.info(s"PATCHED: $dotName (buffered, ${buffered.length} bytes)")
      return buffered
    }

    // Quick field scan: only proceed if class has $lzy or OFFSET$ fields
    val relevant = hasRelevantFields(classfileBuffer)
    if (!relevant) {
      scribe.debug(s"transform($dotName): no relevant fields, skipping")
      return null
    }

    // Null loader means bootstrap classloader — already filtered by skipPrefixes
    if (loader == null) {
      scribe.debug(s"transform($dotName): null loader, skipping")
      return null
    }

    scribe.debug(s"transform($dotName): has relevant fields, attempting patch (loader=${loader.getClass.getName})")

    try {
      // Compute companion name: Foo$ <-> Foo
      val companionDotName =
        if (dotName.endsWith("$")) dotName.stripSuffix("$")
        else dotName + "$"

      // Load companion bytes via getResourceAsStream (pure I/O, no class definition)
      val companionInternalName = companionDotName.replace('.', '/')
      val companionBytes: Option[Array[Byte]] =
        try {
          val stream = loader.getResourceAsStream(companionInternalName + ".class")
          if (stream != null) {
            try {
              val bytes = Some(stream.readAllBytes())
              scribe.debug(s"  companion $companionDotName: found (${bytes.get.length} bytes)")
              bytes
            } finally stream.close()
          } else {
            scribe.debug(s"  companion $companionDotName: not found (getResourceAsStream returned null)")
            None
          }
        } catch {
          case t: Throwable =>
            scribe.debug(s"  companion $companionDotName: error reading: ${t.getMessage}")
            None
        }

      // Build classfile map (dot-separated names)
      val classfileMap: Map[String, Array[Byte]] = companionBytes match {
        case Some(bytes) => Map(dotName -> classfileBuffer, companionDotName -> bytes)
        case None        => Map(dotName -> classfileBuffer)
      }

      scribe.debug(s"  classfileMap keys: ${classfileMap.keys.mkString(", ")}")

      // Group and patch
      LazyValAnalyzer.group(classfileMap) match {
        case Left(error) =>
          scribe.warn(s"Failed to group $dotName: $error")
          null

        case Right(groups) =>
          scribe.debug(
            s"  group() returned ${groups.size} group(s): ${groups.map(g => s"${g.getClass.getSimpleName}(${g.primaryName})").mkString(", ")}"
          )

          // There should be exactly one group (single or companion pair)
          groups.headOption.map(BytecodePatcher.patch(_, classLoader = Some(loader))) match {
            case Some(BytecodePatcher.PatchResult.PatchedSingle(name, patchedBytes)) =>
              scribe.info(s"PATCHED: $dotName (single, ${patchedBytes.length} bytes)")
              dumpBytes(name, patchedBytes)
              patchedBytes

            case Some(BytecodePatcher.PatchResult.PatchedPair(objName, clsName, objBytes, clsBytes)) =>
              // Determine which side is the current class and buffer the other
              val (currentBytes, companionName, companionPatchedBytes) =
                if (dotName == objName) (objBytes, clsName, clsBytes)
                else (clsBytes, objName, objBytes)

              patchedCompanionBuffer.put(companionName, companionPatchedBytes)
              scribe.info(s"PATCHED: $dotName (pair, buffered=$companionName)")
              dumpBytes(objName, objBytes)
              dumpBytes(clsName, clsBytes)
              currentBytes

            case Some(BytecodePatcher.PatchResult.NotApplicable) =>
              scribe.debug(s"  patch() -> NotApplicable")
              null

            case Some(BytecodePatcher.PatchResult.Failed(error)) =>
              scribe.error(s"FATAL: Failed to patch $dotName:\n$error")
              throw new sloth.patching.LazyValPatchingException(s"Failed to patch $dotName:\n$error")

            case None =>
              scribe.debug(s"  no groups to patch")
              null
          }
      }
    } catch {
      case t: sloth.patching.LazyValPatchingException =>
        // Re-throw diagnostic exceptions — the class must not load with broken bytecode
        throw t
      case t: Throwable =>
        scribe.error(s"EXCEPTION in transform($dotName): ${t.getClass.getName}: ${t.getMessage}")
        scribe.debug {
          val sw = new java.io.StringWriter()
          t.printStackTrace(new java.io.PrintWriter(sw))
          s"  stack trace:\n$sw"
        }
        // Never throw from a ClassFileTransformer — return null to leave class unchanged
        null
    }
  }

  /** Dump patched bytes to /tmp for post-mortem inspection (only at trace level). */
  private def dumpBytes(name: String, bytes: Array[Byte]): Unit = {
    if (config.logLevel.value <= scribe.Level.Trace.value) {
      try {
        val pid = ProcessHandle.current().pid()
        val safeName = name.replace('.', '_').replace('$', '_')
        val dumpPrefix = new StringBuilder("sl").append("oth").toString
        val path = java.nio.file.Paths.get(s"/tmp/$dumpPrefix-dump-$pid-$safeName.class")
        java.nio.file.Files.write(path, bytes)
        scribe.trace(s"  dumped ${bytes.length} bytes to $path")
      } catch {
        case t: Throwable => scribe.trace(s"  failed to dump bytes: ${t.getMessage}")
      }
    }
  }

  /** Quick check for relevant fields using ASM with SKIP_CODE | SKIP_DEBUG. */
  private def hasRelevantFields(bytes: Array[Byte]): Boolean = {
    var found = false
    try {
      val reader = new ClassReader(bytes)
      reader.accept(
        new ClassVisitor(Opcodes.ASM9) {
          override def visitField(
              access: Int,
              name: String,
              descriptor: String,
              signature: String,
              value: Any
          ): FieldVisitor = {
            if (!found) {
              if (name.contains("$lzy") && !name.contains("$lzyHandle")) {
                found = true
              } else if (descriptor == "J" && (name.startsWith("OFFSET$_m_") || name.startsWith("OFFSET$"))) {
                found = true
              }
            }
            null // Don't visit field annotations
          }
        },
        ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG
      )
    } catch {
      case _: Throwable => // Malformed classfile — skip
    }
    found
  }
}
