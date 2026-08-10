package sloth.agent

import java.lang.instrument.Instrumentation

/** Java agent entry point for Sloth.
  *
  * Patches Scala 3.0-3.7 lazy val bytecode at class-load time to use VarHandle-based implementation (3.8+ format),
  * avoiding sun.misc.Unsafe.
  *
  * Usage: java -javaagent:sloth-agent.jar[=options] -jar app.jar
  *
  * Options (comma-separated):
  *   - verbose: Log patched classes and internals to stderr (Debug level)
  *   - trace: Log everything including byte dumps to stderr (Trace level)
  *   - include=com.example.: Only transform matching packages
  *   - exclude=com.example.internal.: Skip matching packages
  */
object SlothAgent {

  /** Log prefix constructed to survive sbt-assembly shade rules. */
  val logPrefix: String = Array("[sl", "oth]").mkString

  private val instanceCounter = new java.util.concurrent.atomic.AtomicInteger(0)

  def premain(agentArgs: String, inst: Instrumentation): Unit = {
    val count = instanceCounter.incrementAndGet()
    if (count > 1) {
      val msg = s"$logPrefix FATAL: premain called $count times — agent loaded more than once. " +
        "This usually means -javaagent is specified alongside JAVA_TOOL_OPTIONS containing the same agent. Remove one."
      System.err.println(msg)
      throw new RuntimeException(msg)
    }

    val config = AgentConfig.parse(agentArgs)

    // Configure scribe: plain single-line format to stderr, level from config
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(
        writer = scribe.writer.SystemErrWriter,
        formatter = scribe.format.Formatter.simple,
        minimumLevel = Some(config.logLevel)
      )
      .replace()

    val transformer = new SlothTransformer(config)
    inst.addTransformer(transformer)

    scribe.info(s"$logPrefix Agent installed")
  }
}
