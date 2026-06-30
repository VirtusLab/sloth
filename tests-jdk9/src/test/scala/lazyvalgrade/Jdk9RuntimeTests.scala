package lazyvalgrade

import munit.FunSuite
import scala.compiletime.uninitialized

/** Java-9 runtime proof for the published artifacts.
  *
  * The agent rewrites Unsafe-based lazy vals into the VarHandle-based scheme, and VarHandle is a
  * Java 9 API. This suite runs the agent (built at `-release:9`) against Scala 3.0-3.7 bytecode on
  * whatever JVM is hosting the tests and asserts the application still produces correct output. When
  * this suite runs on a Java 9 JVM (the `tests-jdk9` CI job), a green run proves the patched
  * VarHandle bytecode verifies and executes on Java 9 — i.e. the published artifacts work there.
  *
  * Unlike the JDK-24+ suites, this one makes NO assertions about `sun.misc.Unsafe` warnings (Java 9
  * does not emit them); it only checks correctness, so it is valid on any JDK >= 9.
  */
class Jdk9RuntimeTests extends FunSuite with ExampleLoader {

  override val munitTimeout = scala.concurrent.duration.Duration(600, "s")

  override val examplesDir: os.Path = os.pwd / "tests" / "src" / "test" / "resources" / "fixtures" / "examples"
  override val testWorkspace: os.Path = os.temp.dir(prefix = "lazyvalgrade-jdk9-tests-", deleteOnExit = false)
  override val quietCompilation: Boolean = true
  override val quietTests: Boolean = true

  /** Only the versions we actually run on Java 9. We exclude:
    *   - 3.0-3.2: the agent rewrites their bitmap lazy vals to the full 3.8 scheme, which needs the
    *     3.8 scala-library runtime helpers — and that library is v61 (Java 17), unloadable on Java 9.
    *   - 3.8.1: its compiler can't emit < v61, so the fixture itself can't run on Java 9.
    * That leaves the 3.3-3.7 Unsafe->VarHandle path, whose own scala-library is v53 and ships the
    * runtime control-state classes the patched bytecode needs.
    */
  val runtimeVersions: Seq[String] = Seq("3.3.0", "3.3.6", "3.4.3", "3.5.2", "3.6.4", "3.7.3")

  override def requiredScalaVersions: Seq[String] = runtimeVersions

  var agentJarPath: os.Path = uninitialized

  override def beforeAll(): Unit = {
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(minimumLevel = Some(scribe.Level.Warn))
      .replace()

    agentJarPath = TestPaths.findAgentJar()
  }

  lazy val examples: Seq[LoadedExample] = loadSelectedExamples()

  // Examples with lazy vals AND a known expected output (cheap: metadata only).
  private val runnableExamples = discoveredExamples.filter { ex =>
    isExampleSelected(ex.name) &&
    ex.metadata.expectedClasses.exists(_.lazyVals.nonEmpty) &&
    ex.metadata.expectedOutput.isDefined
  }

  /** Full classpath (scala-library + deps) for a compiled example, via scala-cli. */
  def getScalaCliClasspath(targetDir: os.Path, scalaVersion: String): String =
    os.proc("scala-cli", "compile", "--print-classpath", "--jvm", "17", "--bloop-startup-timeout", "180s", "-S", scalaVersion, targetDir.toString)
      .call(cwd = targetDir, stderr = os.Pipe, stdout = os.Pipe)
      .out.text().trim

  /** Run a main class with the agent attached on the host JVM. */
  def runWithAgent(outputDir: os.Path, scalaLibClasspath: String, mainClass: String): (Int, String, String) = {
    val fullClasspath = s"$outputDir:$scalaLibClasspath"
    val result = os
      .proc("java", s"-javaagent:$agentJarPath", "-cp", fullClasspath, mainClass)
      .call(check = false, stderr = os.Pipe)
    (result.exitCode, result.out.text().trim, result.err.text().trim)
  }

  for {
    discovered <- runnableExamples
    version <- runtimeVersions
    if isScalaVersionSelected(version)
  } {
    test(s"Java-9 runtime (agent): ${discovered.name} with $version") {
      val example = examples.find(_.name == discovered.name).get
      val expectedOutput = example.metadata.expectedOutput.get
      val mainClassName = example.metadata.mainClassName

      example.compilationResult.results.get(version) match {
        case Some(versionResult) if versionResult.success =>
          val outputDir = os.Path(versionResult.classFiles.head.absolutePath.toNIO.getParent)
          val targetDir = testWorkspace / example.name / version
          val scalaLibClasspath = getScalaCliClasspath(targetDir, version)
          val (exitCode, stdout, stderr) = runWithAgent(outputDir, scalaLibClasspath, mainClassName)

          assertEquals(
            exitCode,
            0,
            s"[${discovered.name}/$version] agent run should exit cleanly (no VerifyError) on this JVM, stderr: $stderr"
          )
          assertEquals(stdout, expectedOutput, s"[${discovered.name}/$version] agent run should produce correct output")

        case _ =>
          fail(s"Version $version was not successfully compiled for ${discovered.name}")
      }
    }
  }
}
