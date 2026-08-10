package sloth.patching

/** Thrown when lazy val bytecode patching fails due to an unrecognized pattern.
  *
  * Contains a detailed diagnostic message with class fields, methods, and per-lazy-val version breakdown to aid in
  * debugging and creating test fixtures.
  */
class LazyValPatchingException(val diagnostic: String) extends RuntimeException(diagnostic)
