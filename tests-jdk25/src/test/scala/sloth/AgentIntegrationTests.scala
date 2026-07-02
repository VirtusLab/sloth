package sloth

import munit.FunSuite
import scala.compiletime.uninitialized

/** End-to-end integration tests for the sloth java agent.
  *
  * Verifies that attaching `-javaagent:sloth-agent.jar` to a JVM running
  * Scala 3.3.6 code eliminates sun.misc.Unsafe warnings and produces correct output.
  *
  * Requires Java 24+ (when Unsafe warnings appear).
  */
class AgentIntegrationTests extends FunSuite {

  override val munitTimeout = scala.concurrent.duration.Duration(600, "s")

  val scalaVersion = "3.3.6"

  val testSource: String =
    """object AgentTestApp:
      |  lazy val greeting: String =
      |    val result = "lazy-val-output-ok"
      |    result
      |  def main(args: Array[String]): Unit =
      |    println(greeting)
      |""".stripMargin

  var agentJarPath: os.Path = uninitialized
  var classpath: String = uninitialized
  var tempDir: os.Path = uninitialized

  override def beforeAll(): Unit = {
    // Verify Java 24+
    val javaVersionOutput = os.proc("java", "--version").call().out.text().trim
    val javaVersionLine = javaVersionOutput.linesIterator.toSeq.headOption.getOrElse("")

    val versionPattern = """(?:java|openjdk)(?:\s+version)?\s+(\d+).*""".r
    javaVersionLine match {
      case versionPattern(major) =>
        val majorVersion = major.toInt
        if (majorVersion < 24) {
          throw new RuntimeException(s"Java 24+ required for agent integration tests (Unsafe warnings), found Java $majorVersion")
        }
      case _ =>
        throw new RuntimeException(s"Could not parse Java version from: $javaVersionLine")
    }

    // Agent jar is built automatically by sbt (Test/test dependsOn agent/assembly)
    agentJarPath = TestPaths.findAgentJar()

    // Write test source and compile with scala-cli. Boot the Bloop daemon once first (this suite
    // doesn't go through ExampleRunner) so the scala-cli calls connect to a running server.
    BloopWarmup.ensure()
    tempDir = os.temp.dir(prefix = "sloth-agent-integ-", deleteOnExit = false)
    os.write(tempDir / "AgentTestApp.scala", testSource)

    println(s"Compiling test source with Scala $scalaVersion...")
    os.proc("scala-cli", "compile", "--jvm", "17", "--bloop-startup-timeout", "180s", "-S", scalaVersion, tempDir.toString)
      .call(cwd = tempDir, stdout = os.Inherit, stderr = os.Inherit)

    // Get classpath
    val cpResult = os.proc("scala-cli", "compile", "--print-classpath", "--jvm", "17", "--bloop-startup-timeout", "180s", "-S", scalaVersion, tempDir.toString)
      .call(cwd = tempDir, stderr = os.Pipe, stdout = os.Pipe)
    classpath = cpResult.out.text().trim

    println(s"Test setup complete. Agent jar: $agentJarPath")
    println(s"Temp dir: $tempDir")
  }

  test("Running without agent produces Unsafe warnings") {
    val result = os.proc("java", "-cp", classpath, "AgentTestApp")
      .call(check = false, stderr = os.Pipe)

    val stdout = result.out.text().trim
    val stderr = result.err.text().trim

    assertEquals(stdout, "lazy-val-output-ok", "Should produce correct output without agent")
    assert(
      stderr.contains("WARNING") && stderr.contains("sun.misc.Unsafe"),
      s"Should have Unsafe warning in stderr without agent, but got: $stderr"
    )
  }

  // Check that stderr has no Unsafe warnings from the application's own scala runtime.
  // The agent's shaded internal runtime (sloth.shaded.scala.runtime.LazyVals$) may
  // still trigger Unsafe warnings since the agent itself is compiled with Scala 3.7.x,
  // but those don't affect the user's application.
  //
  // JVM Unsafe warnings mention the calling class like:
  //   "WARNING: ... has been called by scala.runtime.LazyVals$ ..."
  // For the agent's own shaded runtime, it would say "sloth.shaded.scala.runtime.LazyVals$".
  // We look for any "has been called by" line that mentions Unsafe-using code
  // but is NOT from our shaded runtime.
  private def hasAppUnsafeWarning(stderr: String): Boolean =
    stderr.linesIterator.exists { line =>
      line.contains("has been called by") &&
      line.contains("LazyVals") &&
      !line.contains("sloth.shaded.")
    }

  test("Running with agent eliminates Unsafe warnings") {
    val result = os.proc("java", s"-javaagent:$agentJarPath", "-cp", classpath, "AgentTestApp")
      .call(check = false, stderr = os.Pipe)

    val stdout = result.out.text().trim
    val stderr = result.err.text().trim

    assert(
      stdout.contains("lazy-val-output-ok"),
      s"Should produce correct output with agent, but got stdout: $stdout"
    )
    assert(
      !hasAppUnsafeWarning(stderr),
      s"Should NOT have application Unsafe warning with agent, but got: $stderr"
    )
  }

  test("Running with agent in verbose mode logs patched class") {
    val result = os.proc("java", s"-javaagent:$agentJarPath=verbose", "-cp", classpath, "AgentTestApp")
      .call(check = false, stderr = os.Pipe)

    val stdout = result.out.text().trim
    val stderr = result.err.text().trim

    assert(
      stdout.contains("lazy-val-output-ok"),
      s"Should produce correct output with verbose agent, but got stdout: $stdout"
    )
    assert(
      stderr.contains("[sloth]"),
      s"Verbose mode should log agent activity, but got: $stderr"
    )
    assert(
      !hasAppUnsafeWarning(stderr),
      s"Should NOT have application Unsafe warning with agent, but got: $stderr"
    )
  }
}
