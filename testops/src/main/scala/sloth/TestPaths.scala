package sloth

/** Small filesystem/bytecode helpers shared by the test modules.
  *
  * Kept here (testops/src/main) so both tests-jdk11 and tests-jdk25 can use it via dependsOn(testops),
  * and so paths stay version-agnostic instead of hard-coding the current Scala target directory.
  */
object TestPaths {

  /** Locate the assembled agent jar under agent/target/ without hard-coding the Scala version in
    * the path. The agent sets crossPaths := false so the jar lands directly in agent/target/, but
    * we search recursively to stay robust against build layout changes.
    */
  def findAgentJar(): os.Path = {
    val agentTarget = os.pwd / "agent" / "target"
    val candidates =
      if os.exists(agentTarget) then
        os.walk(agentTarget).filter(p => os.isFile(p) && p.last == "sloth-agent.jar")
      else Seq.empty
    candidates.headOption.getOrElse(
      throw new RuntimeException(
        s"Agent jar not found under $agentTarget (expected sloth-agent.jar; should be built by sbt via agent/assembly)"
      )
    )
  }

  /** The classfile major version of a single .class byte array (bytes 6-7, big-endian).
    * 52 = Java 8, 53 = Java 9, 61 = Java 17.
    */
  def classMajorVersion(classBytes: Array[Byte]): Int =
    ((classBytes(6) & 0xff) << 8) | (classBytes(7) & 0xff)

  /** Major version for every `.class` entry in a jar, keyed by entry name. */
  def jarClassMajorVersions(jar: os.Path): Map[String, Int] = {
    import java.util.zip.ZipInputStream
    val result = scala.collection.mutable.Map.empty[String, Int]
    val in = new ZipInputStream(os.read.inputStream(jar))
    try {
      var entry = in.getNextEntry()
      while (entry != null) {
        if (!entry.isDirectory && entry.getName.endsWith(".class")) {
          // Read just enough bytes to reach the major-version field (offset 8).
          val header = new Array[Byte](8)
          var read = 0
          while (read < 8) {
            val n = in.read(header, read, 8 - read)
            if (n < 0) read = 8 else read += n
          }
          result(entry.getName) = ((header(6) & 0xff) << 8) | (header(7) & 0xff)
        }
        entry = in.getNextEntry()
      }
    } finally in.close()
    result.toMap
  }
}
