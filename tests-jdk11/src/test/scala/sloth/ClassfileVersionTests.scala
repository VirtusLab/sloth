package sloth

import munit.FunSuite

/** Guards that the published artifacts are emitted at Java 9 bytecode level (classfile major
  * version <= 53) so they load on a Java 9 JVM.
  *
  *   - The assembled agent jar bundles our shaded code plus the shaded scala/asm/etc. runtime;
  *     EVERY class in it must be <= v53 for the agent to attach on Java 9.
  *   - The core library's own classes (the other published artifact) must be <= v53.
  *
  * 53 = Java 9, 52 = Java 8 (both fine on a Java 9 runtime); 61 = Java 17 (would fail to load).
  */
class ClassfileVersionTests extends FunSuite {

  private val maxAllowedMajor = 53 // Java 9

  test("assembled agent jar contains only <= Java 9 (v53) bytecode") {
    val agentJar = TestPaths.findAgentJar()
    val versions = TestPaths.jarClassMajorVersions(agentJar)
    assert(versions.nonEmpty, s"No .class entries found in $agentJar")

    val tooNew = versions.filter { case (_, major) => major > maxAllowedMajor }
    assert(
      tooNew.isEmpty,
      s"Agent jar $agentJar has classes above Java 9 (v$maxAllowedMajor); these won't load on Java 9:\n" +
        tooNew.toSeq.sortBy(-_._2).take(20).map { case (n, v) => s"  $n: v$v" }.mkString("\n")
    )
  }

  test("core library classes are <= Java 9 (v53) bytecode") {
    // Use the SAME scala target dir as this test was built into (e.g. "scala-3.3.8"), derived from
    // where this class was loaded from. Globbing scala-* would also pick up stale dirs from earlier
    // builds at a different scalaVersion (e.g. a leftover scala-3.8.1/ with v61 classes).
    val classDir = os.pwd / "core" / "target" / currentScalaTargetDir / "classes"
    assert(os.exists(classDir), s"No compiled core classes found at $classDir")

    val offenders = os.walk(classDir).filter(p => os.isFile(p) && p.last.endsWith(".class")).flatMap { cf =>
      val major = TestPaths.classMajorVersion(os.read.bytes(cf))
      if major > maxAllowedMajor then Some(s"${cf.relativeTo(classDir)}: v$major") else None
    }
    assert(
      offenders.isEmpty,
      s"core has classes above Java 9 (v$maxAllowedMajor):\n  ${offenders.take(20).mkString("\n  ")}"
    )
  }

  /** The scala target directory name of the running build (e.g. "scala-3.3.8"), taken from this
    * test's own code-source path so it always matches the current scalaVersion. */
  private def currentScalaTargetDir: String =
    Option(getClass.getProtectionDomain.getCodeSource)
      .map(cs => os.Path(java.nio.file.Paths.get(cs.getLocation.toURI)))
      .flatMap(_.segments.find(_.startsWith("scala-")))
      .getOrElse(fail("Could not determine current scala target dir from test code source"))
}
