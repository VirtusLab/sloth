inThisBuild(List(
  organization := "org.virtuslab",
  homepage := Some(url("https://github.com/VirtusLab/sloth")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "lbialy",
      "Łukasz Biały",
      "lbialy@virtuslab.com",
      url("https://github.com/VirtusLab")
    )
  )
))

lazy val core = project
  .in(file("core"))
  .settings(
    name := "sloth-core",
    // Scala 3.3 LTS (not 3.8): 3.8's compiler/stdlib require JDK 17 and cannot emit < v61 bytecode,
    // so artifacts built with it can't load on Java 9. 3.3.8's stdlib targets Java 8 and runs on 9.
    scalaVersion := "3.3.8",
    // Publish bytecode at Java 9 level: VarHandle (the patched lazy-val impl) is a Java 9 API, so
    // Java 9 is the runtime floor. -release also pins the API surface to Java 9, catching accidental
    // use of a newer JDK method that would NoSuchMethodError on an old JVM. -Yfuture-lazy-vals opts
    // into the Unsafe-free (VarHandle) lazy-val scheme that became the default in 3.8, so our own
    // code/runtime doesn't reintroduce sun.misc.Unsafe after the downgrade from 3.8.
    scalacOptions ++= Seq("-release", "9", "-Yfuture-lazy-vals"),
    libraryDependencies ++= Seq(
      "org.ow2.asm" % "asm" % "9.7",
      "org.ow2.asm" % "asm-commons" % "9.7",
      "org.ow2.asm" % "asm-util" % "9.7",
      "org.ow2.asm" % "asm-tree" % "9.7",
      "com.outr" %% "scribe" % "3.15.0",
      "com.lihaoyi" %% "os-lib" % "0.11.3"
    )
  )

lazy val testops = project
  .in(file("testops"))
  .settings(
    name := "sloth-testops",
    publish / skip := true,
    scalaVersion := "3.3.8",
    libraryDependencies ++= Seq(
      "com.outr" %% "scribe" % "3.15.0",
      "com.lihaoyi" %% "os-lib" % "0.11.3",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.32.0",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.32.0" % Provided
    ),
    Compile / mainClass := Some("sloth.CompileExamplesMain"),
    assembly / mainClass := Some("sloth.CompileExamplesMain"),
    assembly / assemblyJarName := "sloth-testops.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    }
  )
  .dependsOn(core)

// Shared settings for both test modules. Test sources are split by JDK requirement:
//   - tests-jdk11 : pure bytecode-analysis suites + a runtime proof that the agent and patched
//                   bytecode actually run on Java 9. Run with `sbt tests-jdk11` on a Java 11 JVM.
//   - tests-jdk25 : suites that assert presence/absence of the `sun.misc.Unsafe` warning, which
//                   only newer JDKs emit. Run with `sbt tests-jdk25` on a Java 24+ JVM.
// Both reuse the fixtures under tests/src/test/resources and the ExampleLoader in testops.
val commonTestSettings = Seq(
  publish / skip := true,
  scalaVersion := "3.3.8",
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % "1.0.0" % Test,
    "com.outr" %% "scribe" % "3.15.0",
    "com.lihaoyi" %% "os-lib" % "0.11.3"
  ),
  testFrameworks += new TestFramework("munit.Framework"),
  // The agent jar must be assembled before runtime tests can attach it.
  Test / test := ((Test / test) dependsOn (agent / assembly)).value,
  Test / testOnly := ((Test / testOnly) dependsOn (agent / assembly)).evaluated
)

lazy val testsJdk11 = project
  .in(file("tests-jdk11"))
  .settings(name := "sloth-tests-jdk11")
  .settings(commonTestSettings)
  .dependsOn(core, testops, agent)

lazy val testsJdk25 = project
  .in(file("tests-jdk25"))
  .settings(name := "sloth-tests-jdk25")
  .settings(commonTestSettings)
  .dependsOn(core, testops, agent)

lazy val cli = project
  .in(file("cli"))
  .settings(
    name := "sloth-cli",
    publish / skip := true,
    scalaVersion := "3.3.8",
    // Keep the CLI loadable on Java 9 as well (matches core/agent bytecode target).
    scalacOptions ++= Seq("-release", "9", "-Yfuture-lazy-vals"),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "os-lib" % "0.11.3",
      "com.lihaoyi" %% "fansi" % "0.5.0",
      "com.outr" %% "scribe" % "3.15.0"
    ),
    Compile / mainClass := Some("sloth.cli.Main"),
    assembly / mainClass := Some("sloth.cli.Main"),
    assembly / assemblyJarName := "sloth.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    }
  )
  .dependsOn(core)

lazy val processDeps = taskKey[Classpath]("Process agent dependency JARs to patch lazy vals")

lazy val agent = project
  .in(file("agent"))
  .settings(
    name := "sloth-agent",
    scalaVersion := "3.3.8",
    // Published artifact: target Java 9 bytecode so the agent loads on JVMs as old as Java 9
    // (see core for rationale). -Yfuture-lazy-vals keeps the agent's own lazy vals Unsafe-free.
    // The shaded scala/asm classes are already <= v53, so the whole assembled jar stays loadable
    // on Java 9. Verified by ClassfileVersionTests in tests-jdk11.
    scalacOptions ++= Seq("-release", "9", "-Yfuture-lazy-vals"),
    crossPaths := false,
    autoScalaLibrary := false,
    Compile / packageBin := assembly.value,
    pomPostProcess := { node =>
      import scala.xml._
      import scala.xml.transform._
      new RuleTransformer(new RewriteRule {
        override def transform(node: Node): Seq[Node] = node match {
          case e: Elem if e.label == "dependencies" => NodeSeq.Empty
          case n => n
        }
      }).transform(node).head
    },
    libraryDependencies ++= Seq(
      "com.outr" %% "scribe" % "3.15.0"
    ),
    processDeps := {
      val log = streams.value.log
      val cliJar = (cli / assembly).value
      val deps = (Compile / dependencyClasspath).value
      val processedDir = target.value / "processed-deps"
      IO.createDirectory(processedDir)

      val depJars = deps.files.filter(_.getName.endsWith(".jar"))
      // Build full classpath: CLI jar + all dependency JARs so ASM can resolve class hierarchies
      val fullCp = (cliJar +: depJars).map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)

      val debugAssembly = sys.env.contains("DEBUG_AGENT_ASSEMBLY")
      val processLogger: scala.sys.process.ProcessLogger = if (debugAssembly)
        scala.sys.process.ProcessLogger(s => log.info(s), s => log.error(s))
      else scala.sys.process.ProcessLogger(_ => (), _ => ())

      val processedFiles = depJars.map { depJar =>
        val dest = processedDir / depJar.getName
        IO.copyFile(depJar, dest)
        if (debugAssembly) log.info(s"Processing ${depJar.getName}...")
        val exitCode = scala.sys.process.Process(
          Seq("java", "-cp", fullCp, "sloth.cli.Main", dest.getAbsolutePath)
        ).!(processLogger)
        if (exitCode != 0) {
          throw new MessageOnlyException(s"Failed to process ${depJar.getName} (exit code $exitCode)")
        }
        Attributed.blank(dest)
      }

      processedFiles
    },
    assembly / fullClasspath := {
      val processed = processDeps.value
      val deps = (Compile / dependencyClasspath).value
      val ownProducts = (Compile / products).value.map(Attributed.blank)
      // Keep non-JAR entries (class directories from project dependencies like core)
      val nonJarDeps = deps.filterNot(_.data.getName.endsWith(".jar"))
      ownProducts ++ nonJarDeps ++ processed
    },
    assembly / assemblyJarName := "sloth-agent.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    },
    assembly / assemblyShadeRules := Seq(
      ShadeRule.rename("sloth.**" -> "sloth.shaded.agent.@0").inAll,
      ShadeRule.rename("scala.**" -> "sloth.shaded.scala.@1").inAll,
      ShadeRule.rename("org.objectweb.asm.**" -> "sloth.shaded.asm.@1").inAll,
      ShadeRule.rename("scribe.**" -> "sloth.shaded.scribe.@1").inAll,
      ShadeRule.rename("perfolation.**" -> "sloth.shaded.perfolation.@1").inAll,
      ShadeRule.rename("moduload.**" -> "sloth.shaded.moduload.@1").inAll,
      ShadeRule.rename("sourcecode.**" -> "sloth.shaded.sourcecode.@1").inAll,
      ShadeRule.rename("com.lihaoyi.**" -> "sloth.shaded.lihaoyi.@1").inAll,
      ShadeRule.rename("os.**" -> "sloth.shaded.os.@1").inAll,
      ShadeRule.rename("geny.**" -> "sloth.shaded.geny.@1").inAll
    ),
    assembly / packageOptions += Package.ManifestAttributes(
      "Premain-Class" -> "sloth.shaded.agent.sloth.agent.SlothAgent",
      "Can-Retransform-Classes" -> "false",
      "Can-Redefine-Classes" -> "false"
    )
  )
  .dependsOn(core)

lazy val agentInstall = taskKey[Unit]("Build agent assembly and install to ~/.sloth/agent.jar")

lazy val root = project
  .in(file("."))
  .settings(
    name := "sloth",
    publish / skip := true,
    scalaVersion := "3.3.8",
    agentInstall := {
      val assembled = (agent / assembly).value
      val target = Path.userHome / ".sloth" / "agent.jar"
      IO.createDirectory(target.getParentFile)
      IO.copyFile(assembled, target)
      streams.value.log.info(s"Installed agent to $target")
    },
    // The test suites are split by JDK requirement and must run on different JVMs, so a single
    // `sbt test` is meaningless (and would silently run JDK-24+ suites on whatever JVM is present).
    // Refuse it and point at the two real targets.
    Test / test := {
      sys.error(
        "`sbt test` is disabled for this build because tests are split by required JVM.\n" +
          "  - `sbt tests-jdk11` : bytecode-analysis + Java-9 runtime suites (run on a Java 11 JVM)\n" +
          "  - `sbt tests-jdk25` : sun.misc.Unsafe warning suites (run on a Java 24+ JVM)"
      )
    },
    addCommandAlias("compileExamples", "testops/runMain sloth.CompileExamplesMain"),
    addCommandAlias("compileExamplesWithPatching", "testops/runMain sloth.CompileExamplesMain --patch"),
    addCommandAlias("tests-jdk11", "testsJdk11/test"),
    addCommandAlias("tests-jdk25", "testsJdk25/test")
  )
  // Note: test modules are intentionally NOT aggregated, so `sbt test` only hits the root's
  // disabled `Test / test` above rather than transitively running them on the wrong JVM.
  .aggregate(core, testops, cli, agent)
