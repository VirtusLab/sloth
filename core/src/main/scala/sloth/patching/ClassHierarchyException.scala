package sloth.patching

/** Thrown in strict mode when a type cannot be resolved while recomputing stack map frames.
  *
  * Widening such a merge to `java/lang/Object` produces frames that no longer describe the code, which the JVM only
  * rejects later, at class load time, as a `VerifyError`.
  */
class ClassHierarchyException(val internalName: String)
    extends RuntimeException(
      s"Cannot resolve class '$internalName' while computing stack map frames. " +
        "Supply the jars or class directories that define it as hierarchy classpath."
    )
