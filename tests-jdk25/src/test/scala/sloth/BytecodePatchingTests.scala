package sloth

import munit.FunSuite
import sloth.classfile.ClassfileParser
import sloth.lazyval.{LazyValDetector, LazyValDetectionResult, ScalaVersion, SemanticLazyValComparator}
import sloth.patching.BytecodePatcher
import sloth.analysis.{LazyValAnalyzer, ClassfileGroup}
import java.nio.file.{Files, Paths, Path}
import scala.util.{Try, Using, boundary}
import scala.util.boundary.break
import scala.collection.immutable.TreeSet
import scala.compiletime.uninitialized

/** End-to-end test suite for bytecode patching.
  *
  * Tests the complete patching pipeline:
  *   1. Compile examples with multiple Scala versions (3.0-3.8) 2. Patch 3.3-3.7 bytecode to 3.8 format 3. Semantically
  *      compare patched versions with real 3.8 4. Runtime testing: verify Unsafe warnings and correct output
  *
  * Use SELECT_EXAMPLE environment variable to filter examples:
  *   - SELECT_EXAMPLE=simple-lazy-val (single example)
  *   - SELECT_EXAMPLE=simple-lazy-val,class-lazy-val (multiple examples)
  */
class BytecodePatchingTests extends FunSuite with ExampleLoader {

  // Increase timeout for tests that compile multiple Scala versions
  override val munitTimeout = scala.concurrent.duration.Duration(600, "s")

  // ===== ExampleLoader implementation =====

  override val examplesDir: os.Path = os.pwd / "tests" / "src" / "test" / "resources" / "fixtures" / "examples"
  override val testWorkspace: os.Path = os.temp.dir(prefix = "sloth-patching-tests-", deleteOnExit = false)
  override val quietCompilation: Boolean = true

  /** Scala versions for testing */
  val testVersions: Seq[String] = Seq(
    "3.0.2",
    "3.1.3",
    "3.2.2",
    "3.3.0",
    "3.3.6",
    "3.4.3",
    "3.5.2",
    "3.6.4",
    "3.7.3",
    "3.8.1"
  )

  override def requiredScalaVersions: Seq[String] = testVersions

  /** Versions that need patching (3.0-3.1, 3.3-3.7) */
  val patchableVersions: Set[String] = Set(
    "3.0.2",
    "3.1.3",
    "3.2.2",
    "3.3.0",
    "3.3.6",
    "3.4.3",
    "3.5.2",
    "3.6.4",
    "3.7.3"
  )

  /** Versions that generate full 3.8+ code and need 3.8+ runtime classes (NullValue$, Evaluating$, etc.) */
  val needsTargetRuntime: Set[String] = Set("3.0.2", "3.1.3", "3.2.2")

  /** Target version (3.8) */
  val targetVersion: String = "3.8.1"

  /** Patched workspace for transformed classfiles */
  val patchedWorkspace: os.Path = testWorkspace / "patched"

  val cleanupWorkspace: Boolean = false

  var agentJarPath: os.Path = uninitialized

  override val quietTests: Boolean = false // BytecodePatchingTests is quiet by default

  /** Helper to print only when not in quiet mode */
  private def log(msg: => String): Unit = if !quietTests then println(msg)

  override def beforeAll(): Unit = {
    // Configure scribe logging
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(minimumLevel = Some(scribe.Level.Warn))
      .replace()

    // Verify Java version
    val javaVersionOutput = os.proc("java", "--version").call().out.text().trim
    val javaVersionLine = javaVersionOutput.linesIterator.toSeq.headOption.getOrElse("")

    log(s"Java version: $javaVersionLine")

    // Check if it's Java 24+ (required for Unsafe warning message)
    // Parse versions like "openjdk 17.0.9", 'openjdk version "25"', "java 25", "openjdk 25 2025-09-16"
    // Match the first number that appears after "java" or "openjdk"
    val versionPattern = """(?:java|openjdk)(?:\s+version)?\s+(\d+).*""".r
    javaVersionLine match {
      case versionPattern(major) =>
        val majorVersion = major.toInt
        if (majorVersion < 24) {
          throw new RuntimeException(s"Java 24+ required for runtime tests (Unsafe warnings), found Java $majorVersion")
        }
        log(s"✓ Using Java $majorVersion")
      case _ =>
        throw new RuntimeException(s"Could not parse Java version from: $javaVersionLine")
    }

    // Agent jar is built automatically by sbt (Test/test dependsOn agent/assembly)
    agentJarPath = TestPaths.findAgentJar()
    log(s"✓ Agent jar: $agentJarPath")
  }

  /** Load and compile all examples */
  lazy val examples: Seq[LoadedExample] = {
    val fixturesDir = os.pwd / "tests" / "src" / "test" / "resources" / "fixtures"

    log(s"Fixtures directory: $fixturesDir")
    log(s"Examples directory: $examplesDir")
    log(s"Test workspace: $testWorkspace")
    log(s"Patched workspace: $patchedWorkspace")

    os.makeDir.all(patchedWorkspace)

    log(s"Discovered ${discoveredExamples.size} examples: ${discoveredExamples.map(_.name).mkString(", ")}")

    val loaded = loadSelectedExamples()
    log(s"Loaded ${loaded.size} examples for testing")
    loaded
  }

  override def afterAll(): Unit = {
    if os.exists(testWorkspace) && cleanupWorkspace then
      log(s"Cleaning up test workspace: $testWorkspace")
      os.remove.all(testWorkspace)
  }

  /** Finds a compiled classfile for a specific example, Scala version, and class name. */
  def findClassFile(example: LoadedExample, scalaVersion: String, className: String): Option[Path] = {
    example.compilationResult.results.get(scalaVersion).flatMap { versionResult =>
      versionResult.classFiles.find(cf => cf.relativePath == s"$className.class" || cf.relativePath.endsWith(s"/$className.class")).map(_.absolutePath.toNIO)
    }
  }

  /** Patches all classfiles for a given example and version, handling companion pairs.
    *
    * @return
    *   Map from original className to patched file paths
    */
  def patchAllClassFilesForVersion(example: LoadedExample, version: String): Either[String, Map[String, Seq[Path]]] = {
    val versionResult = example.compilationResult.results.get(version)
    if (versionResult.isEmpty || !versionResult.get.success) {
      Left(s"Version $version not successfully compiled")
    } else {
      // Collect all classfiles for this version
      val classFiles = versionResult.get.classFiles

      // Read all classfiles into memory with their class names
      val classfileMap = classFiles.map { cf =>
        val bytes = Files.readAllBytes(cf.absolutePath.toNIO)
        val className = cf.relativePath.stripSuffix(".class").replace("/", ".")
        (className, bytes)
      }.toMap

      // Build a classloader from the classes base directory for class hierarchy resolution
      val classesBaseUrls = classFiles.headOption.map { cf =>
        // Derive the base classes directory by stripping the relative path from the absolute path
        val absStr = cf.absolutePath.toString
        val relStr = cf.relativePath
        val baseDir = absStr.stripSuffix(relStr)
        java.nio.file.Paths.get(baseDir).toUri.toURL
      }.toArray
      val classLoader = new java.net.URLClassLoader(classesBaseUrls, getClass.getClassLoader)

      // Group classfiles (detect companion pairs)
      LazyValAnalyzer.group(classfileMap).flatMap { groups =>
        // Validate that we can detect versions for all groups (no Unknown or MixedVersions in tests)
        boundary {
          groups.foreach { group =>
            val detectionResult = group match {
              case ClassfileGroup.Single(name, classInfo, _) =>
                LazyValDetector.detect(classInfo, None)
              case ClassfileGroup.CompanionPair(_, _, companionObjectInfo, classInfo, _, _) =>
                LazyValDetector.detect(companionObjectInfo, Some(classInfo))
            }

            detectionResult match {
              case LazyValDetectionResult.NoLazyVals => // OK
              case LazyValDetectionResult.LazyValsFound(lazyVals, ScalaVersion.Unknown(reason)) =>
                break(Left(s"Detected Unknown version for ${group.primaryName} compiled with Scala $version: $reason. LazyVals: ${lazyVals.map(lv => s"${lv.name} (version=${lv.version})").mkString(", ")}"))
              case LazyValDetectionResult.LazyValsFound(_, _) => // OK
              case LazyValDetectionResult.MixedVersions(lazyVals) =>
                val versionBreakdown = lazyVals.groupBy(_.version).map { case (v, lvs) => s"$v: ${lvs.map(_.name).mkString(", ")}" }.mkString("; ")
                break(Left(s"Detected mixed versions for ${group.primaryName} compiled with Scala $version. Breakdown: $versionBreakdown"))
            }
          }

          // Patch each group
          val resultMap = scala.collection.mutable.Map[String, Seq[Path]]()
          var error: Option[String] = None

          groups.foreach { group =>
            if (error.isEmpty) {
              BytecodePatcher.patch(group, classLoader = Some(classLoader)) match {
                case BytecodePatcher.PatchResult.PatchedSingle(name, bytes) =>
                  // Write single patched file
                  val patchedPath = writePatchedFile(name, bytes, example, version)
                  resultMap(name) = Seq(patchedPath)

                case BytecodePatcher.PatchResult
                      .PatchedPair(companionObjectName, className, companionObjectBytes, classBytes) =>
                  // Write both patched files
                  val objectPath = writePatchedFile(companionObjectName, companionObjectBytes, example, version)
                  val classPath = writePatchedFile(className, classBytes, example, version)
                  resultMap(companionObjectName) = Seq(objectPath)
                  resultMap(className) = Seq(classPath)

                case BytecodePatcher.PatchResult.NotApplicable =>
                  // Skip, no patching needed
                  ()

                case BytecodePatcher.PatchResult.Failed(err) =>
                  error = Some(s"Patching failed for group ${group.primaryName}: $err")
              }
            }
          }

          error match {
            case Some(err) => Left(err)
            case None      => Right(resultMap.toMap)
          }
        }
      }
    }
  }

  /** Writes a patched classfile to the workspace. */
  private def writePatchedFile(className: String, bytes: Array[Byte], example: LoadedExample, version: String): Path = {
    val relativePath = className.replace('.', '/') + ".class"
    val patchedPath = patchedWorkspace / example.name / version / os.RelPath(relativePath)

    os.makeDir.all(patchedPath / os.up)
    os.write.over(patchedPath, bytes)

    patchedPath.toNIO
  }

  /** Gets the full classpath for a compiled example (includes Scala library).
    *
    * @param targetDir
    *   The directory containing the compiled Scala code
    * @param scalaVersion
    *   The Scala version used for compilation
    * @return
    *   Full classpath string including Scala library and dependencies
    */
  def getScalaCliClasspath(targetDir: os.Path, scalaVersion: String): String = {
    val result = os
      .proc("scala-cli", "compile", "--print-classpath", "--jvm", "17", "--bloop-startup-timeout", "180s", "-S", scalaVersion, targetDir.toString)
      .call(cwd = targetDir, stderr = os.Pipe, stdout = os.Pipe)

    result.out.text().trim
  }

  /** Runs a classfile with java and captures stdout and stderr.
    *
    * @param outputDir
    *   Directory containing compiled class files
    * @param scalaLibClasspath
    *   Classpath for Scala library and dependencies
    * @param mainClass
    *   Main class to run
    * @return
    *   (exitCode, stdout, stderr)
    */
  def runWithJava(outputDir: os.Path, scalaLibClasspath: String, mainClass: String): (Int, String, String) = {
    // Combine the output directory with the Scala library classpath
    val fullClasspath = s"${outputDir}:${scalaLibClasspath}"

    val result = os
      .proc("java", "-cp", fullClasspath, mainClass)
      .call(check = false, stderr = os.Pipe)

    (result.exitCode, result.out.text().trim, result.err.text().trim)
  }

  /** Check that stderr has no Unsafe warnings from the application's own scala runtime.
    * The agent's shaded internal runtime (sloth.shaded.scala.runtime.LazyVals$) may
    * still trigger Unsafe warnings since the agent itself is compiled with Scala 3.7.x,
    * but those don't affect the user's application.
    */
  private def hasAppUnsafeWarning(stderr: String): Boolean =
    stderr.linesIterator.exists { line =>
      line.contains("has been called by") &&
      line.contains("LazyVals") &&
      !line.contains("sloth.shaded.")
    }

  /** Runs a classfile with java agent and captures stdout and stderr. */
  def runWithAgent(outputDir: os.Path, scalaLibClasspath: String, mainClass: String): (Int, String, String) = {
    val fullClasspath = s"${outputDir}:${scalaLibClasspath}"
    val result = os
      .proc("java", s"-javaagent:$agentJarPath", "-cp", fullClasspath, mainClass)
      .call(check = false, stderr = os.Pipe)
    (result.exitCode, result.out.text().trim, result.err.text().trim)
  }

  /** Test: Patch patchable bytecode and verify semantic equivalence with 3.8 */
  test("Patch patchable bytecode and verify semantic equivalence with 3.8") {
    examples.foreach { example =>
      // Skip examples without lazy vals
      if (example.metadata.expectedClasses.exists(_.lazyVals.nonEmpty)) {
        log(s"\n[${example.name}] Testing bytecode patching")

        // Only test patchable versions that were actually compiled
        val compiledPatchableVersions = patchableVersions.filter(v =>
          example.compilationResult.results.get(v).exists(_.success))

        example.metadata.expectedClasses.filter(_.lazyVals.nonEmpty).foreach { expectedClass =>
          val className = expectedClass.className

          // Get 3.8 reference classfile
          val ref38File = findClassFile(example, targetVersion, className)
          assert(ref38File.isDefined, s"3.8 reference classfile not found for $className")

          val ref38Bytes = Files.readAllBytes(ref38File.get)
          val ref38ClassInfo = ClassfileParser.parse(ref38Bytes).toOption.get

          compiledPatchableVersions.foreach { version =>
            // Patch all classfiles for this version (handles companion pairs)
            patchAllClassFilesForVersion(example, version) match {
              case Right(patchedFilesMap) =>
                // Look up patched files for this className
                patchedFilesMap.get(className) match {
                  case Some(patchedFiles) =>
                    log(s"  Testing $version → 3.8 transformation for $className")
                    log(s"    ✓ Patched successfully (${patchedFiles.size} file(s))")

                    // Parse the main patched classfile (first one for this className)
                    val patchedBytes = Files.readAllBytes(patchedFiles.head)
                    val patchedClassInfo = ClassfileParser.parse(patchedBytes).toOption.get

                    // Semantic comparison with 3.8 reference
                    val comparison = SemanticLazyValComparator.compare(patchedClassInfo, ref38ClassInfo)

                    if !comparison.areIdentical then
                      // Inspect bytecode on failure - show both the patched version and the 3.8 reference
                      inspectOnFailure(example, version, s"Patching semantic comparison failure: Expected patched $version to be identical to 3.8 for $className, but got: $comparison")
                      inspectOnFailure(example, targetVersion, s"Patching semantic comparison failure: 3.8 reference for $className")
                      fail(s"Patched $version bytecode should be semantically identical to 3.8 for $className, but got: $comparison")

                    log(s"    ✓ Semantically identical to 3.8")

                  case None =>
                    log(s"  ⊘ Skipping $version (not patched)")
                }

              case Left(error) =>
                fail(s"Failed to patch $className from $version: $error")
            }
          }
        }
      }
    }
  }

  /** Test: Runtime verification - Unsafe warnings and correct output */
  test("Runtime verification: Unsafe warnings and correct output") {
    examples.foreach { example =>
      // Only test examples with lazy vals AND expected output
      if (example.metadata.expectedClasses.exists(_.lazyVals.nonEmpty) && example.metadata.expectedOutput.isDefined) {
        log(s"\n[${example.name}] Testing runtime behavior")

        val expectedOutput = example.metadata.expectedOutput.get
        val mainClassName = example.metadata.mainClassName

        // Only test patchable versions that were actually compiled
        val compiledPatchableVersions = patchableVersions.filter(v =>
          example.compilationResult.results.get(v).exists(_.success))

        // Test pre-patched versions - should have Unsafe warning
        compiledPatchableVersions.foreach { version =>
          example.compilationResult.results.get(version).foreach { versionResult =>
            if (versionResult.success) {
              log(s"  Testing pre-patched $version runtime")

              val anyFile = versionResult.classFiles.head
              val outputDir = os.Path(anyFile.absolutePath.toString.stripSuffix(anyFile.relativePath).stripSuffix("/"))
              val targetDir = testWorkspace / example.name / version
              val scalaLibClasspath = getScalaCliClasspath(targetDir, version)
              val (exitCode, stdout, stderr) = runWithJava(outputDir, scalaLibClasspath, mainClassName)

              // Verify correct output
              assertEquals(
                stdout,
                expectedOutput,
                s"Pre-patched $version should produce correct output"
              )

              // Verify Unsafe warning is present
              assert(
                stderr.contains("WARNING") && stderr.contains("sun.misc.Unsafe") && stderr
                  .contains("scala.runtime.LazyVals"),
                s"Pre-patched $version should have Unsafe warning in stderr, but got: $stderr"
              )

              log(s"    ✓ Correct output and Unsafe warning present")
            }
          }
        }

        // Test patched versions - should NOT have Unsafe warning
        compiledPatchableVersions.foreach { version =>
          patchAllClassFilesForVersion(example, version) match {
            case Right(patchedFilesMap) if patchedFilesMap.nonEmpty =>
              log(s"  Testing patched $version runtime")

              // Use the patched workspace root for this example/version (not the file's parent,
              // which would be wrong for classes in packages like foo.package$)
              val outputDir = patchedWorkspace / example.name / version
              // 3.0-3.1 patched code generates full 3.8+ patterns needing NullValue$, Evaluating$, etc.
              // Use 3.8+ classpath for those versions
              val classpathVersion = if (needsTargetRuntime(version)) targetVersion else version
              val targetDir = testWorkspace / example.name / classpathVersion
              val scalaLibClasspath = getScalaCliClasspath(targetDir, classpathVersion)
              val (exitCode, stdout, stderr) = runWithJava(outputDir, scalaLibClasspath, mainClassName)

              // Verify correct output
              assertEquals(
                stdout,
                expectedOutput,
                s"Patched $version should produce correct output"
              )

              // Verify NO Unsafe warning
              assert(
                !stderr.contains("sun.misc.Unsafe"),
                s"[${example.name}] Patched $version should NOT have Unsafe warning in stderr, but got: $stderr"
              )

              log(s"    ✓ Correct output and NO Unsafe warning")

            case Left(error) =>
              fail(s"[${example.name}/$version] Failed to patch for runtime testing: $error")

            case Right(_) =>
              log(s"  ⊘ No files patched for $version")
          }
        }

        // Test 3.8 reference - should NOT have Unsafe warning
        example.compilationResult.results.get(targetVersion).foreach { versionResult =>
          if (versionResult.success) {
            log(s"  Testing 3.8 reference runtime")

            val anyFile = versionResult.classFiles.head
            val outputDir = os.Path(anyFile.absolutePath.toString.stripSuffix(anyFile.relativePath).stripSuffix("/"))
            val targetDir = testWorkspace / example.name / targetVersion
            val scalaLibClasspath = getScalaCliClasspath(targetDir, targetVersion)
            val (exitCode, stdout, stderr) = runWithJava(outputDir, scalaLibClasspath, mainClassName)

            // Verify correct output
            assertEquals(
              stdout,
              expectedOutput,
              s"3.8 reference should produce correct output"
            )

            // Verify NO Unsafe warning
            assert(
              !stderr.contains("sun.misc.Unsafe"),
              s"3.8 reference should NOT have Unsafe warning in stderr, but got: $stderr"
            )

            log(s"    ✓ Correct output and NO Unsafe warning")
          }
        }
      }
    }
  }

  /** Test: Verify patching is idempotent (patching patched bytecode should be no-op) */
  test("Patching is idempotent") {
    examples.foreach { example =>
      if (example.metadata.expectedClasses.exists(_.lazyVals.nonEmpty)) {
        log(s"\n[${example.name}] Testing idempotency")

        val compiledPatchableVersions = patchableVersions.filter(v =>
          example.compilationResult.results.get(v).exists(_.success))
        compiledPatchableVersions.headOption.foreach { version =>
          patchAllClassFilesForVersion(example, version) match {
            case Right(patchedFilesMap) if patchedFilesMap.nonEmpty =>
              // Try patching the patched files again - read them back and group
              val repatchClassfileMap = patchedFilesMap.flatMap { case (className, paths) =>
                paths.map { path =>
                  val bytes = Files.readAllBytes(path)
                  (className, bytes)
                }
              }

              LazyValAnalyzer.group(repatchClassfileMap) match {
                case Right(groups) =>
                  groups.foreach { group =>
                    BytecodePatcher.patch(group) match {
                      case BytecodePatcher.PatchResult.NotApplicable =>
                        log(s"  ✓ Patched bytecode correctly returns NotApplicable for ${group.primaryName}")

                      case other =>
                        fail(s"Patching already-patched bytecode should return NotApplicable, got: $other")
                    }
                  }

                case Left(error) =>
                  fail(s"Failed to group patched files: $error")
              }

            case Left(error) =>
              fail(s"Failed initial patching: $error")

            case Right(_) =>
              log(s"  ⊘ No files to test idempotency")
          }
        }
      }
    }
  }

  /** Test: Non-patchable versions return NotApplicable */
  test("Non-patchable versions return NotApplicable") {
    val nonPatchableVersions = Set(targetVersion)

    examples.foreach { example =>
      if (example.metadata.expectedClasses.exists(_.lazyVals.nonEmpty)) {
        log(s"\n[${example.name}] Testing non-patchable versions")

        nonPatchableVersions.foreach { version =>
          example.compilationResult.results.get(version).foreach { versionResult =>
            if (versionResult.success) {
              // Read all classfiles for this version
              val classfileMap = versionResult.classFiles.map { cf =>
                val bytes = Files.readAllBytes(cf.absolutePath.toNIO)
                val className = cf.relativePath.stripSuffix(".class").replace("/", ".")
                (className, bytes)
              }.toMap

              // Group and patch
              LazyValAnalyzer.group(classfileMap) match {
                case Right(groups) =>
                  groups.foreach { group =>
                    BytecodePatcher.patch(group) match {
                      case BytecodePatcher.PatchResult.NotApplicable =>
                        log(s"  ✓ $version correctly returns NotApplicable for ${group.primaryName}")

                      case other =>
                        fail(s"$version should return NotApplicable, got: $other")
                    }
                  }

                case Left(error) =>
                  fail(s"Failed to group classfiles for $version: $error")
              }
            }
          }
        }
      }
    }
  }

  // ===== Agent runtime tests (individual per example × version) =====

  // Filter to examples that have lazy vals and expected output (cheap: metadata only, no compilation)
  private val agentTestableExamples = discoveredExamples.filter { ex =>
    isExampleSelected(ex.name) &&
    ex.metadata.expectedClasses.exists(_.lazyVals.nonEmpty) &&
    ex.metadata.expectedOutput.isDefined
  }

  // Register individual agent tests for each example × patchable version
  for {
    discovered <- agentTestableExamples
    version <- patchableVersions.toSeq.sorted
    if isScalaVersionSelected(version)
  } {
    test(s"Agent runtime: ${discovered.name} with $version") {
      val example = examples.find(_.name == discovered.name).get
      val expectedOutput = example.metadata.expectedOutput.get
      val mainClassName = example.metadata.mainClassName

      example.compilationResult.results.get(version) match {
        case Some(versionResult) if versionResult.success =>
          val anyFile = versionResult.classFiles.head
          val outputDir = os.Path(anyFile.absolutePath.toString.stripSuffix(anyFile.relativePath).stripSuffix("/"))
          val classpathVersion = if (needsTargetRuntime(version)) targetVersion else version
          val targetDir = testWorkspace / example.name / classpathVersion
          val scalaLibClasspath = getScalaCliClasspath(targetDir, classpathVersion)
          val (exitCode, stdout, stderr) = runWithAgent(outputDir, scalaLibClasspath, mainClassName)

          assertEquals(stdout, expectedOutput, s"Agent with $version should produce correct output")
          assert(
            !hasAppUnsafeWarning(stderr),
            s"Agent with $version should NOT have application Unsafe warning, but got: $stderr"
          )
          log(s"  ✓ [${discovered.name}/$version] Correct output and no application Unsafe warnings")

        case _ =>
          fail(s"Version $version was not successfully compiled for ${discovered.name}")
      }
    }
  }

  // Register individual agent tests for 3.8+ pass-through
  for {
    discovered <- agentTestableExamples
    if isScalaVersionSelected(targetVersion)
  } {
    test(s"Agent runtime: ${discovered.name} with $targetVersion (pass-through)") {
      val example = examples.find(_.name == discovered.name).get
      val expectedOutput = example.metadata.expectedOutput.get
      val mainClassName = example.metadata.mainClassName

      example.compilationResult.results.get(targetVersion) match {
        case Some(versionResult) if versionResult.success =>
          val anyFile = versionResult.classFiles.head
          val outputDir = os.Path(anyFile.absolutePath.toString.stripSuffix(anyFile.relativePath).stripSuffix("/"))
          val targetDir = testWorkspace / example.name / targetVersion
          val scalaLibClasspath = getScalaCliClasspath(targetDir, targetVersion)
          val (exitCode, stdout, stderr) = runWithAgent(outputDir, scalaLibClasspath, mainClassName)

          assertEquals(stdout, expectedOutput, s"Agent with $targetVersion should produce correct output")
          assert(
            !hasAppUnsafeWarning(stderr),
            s"Agent with $targetVersion should NOT have application Unsafe warning, but got: $stderr"
          )
          log(s"  ✓ [${discovered.name}/$targetVersion] Correct output and no Unsafe warnings")

        case _ =>
          fail(s"Version $targetVersion was not successfully compiled for ${discovered.name}")
      }
    }
  }
}
