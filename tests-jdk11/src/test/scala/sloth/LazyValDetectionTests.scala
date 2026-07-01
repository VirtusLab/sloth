package sloth

import munit.FunSuite
import sloth.classfile.ClassfileParser
import sloth.lazyval.{LazyValDetector, LazyValDetectionResult, ScalaVersion}
import java.nio.file.{Files, Paths, Path}
import scala.util.Try
import scala.collection.immutable.TreeSet

/** Test suite for lazy val detection across Scala versions.
  *
  * Discovers and compiles test fixtures using ExampleRunner, then verifies that the correct lazy val implementation is
  * detected based on metadata.json files in each example.
  *
  * Use SELECT_EXAMPLE environment variable to filter examples:
  *   - SELECT_EXAMPLE=simple-lazy-val (single example)
  *   - SELECT_EXAMPLE=simple-lazy-val,class-lazy-val (multiple examples)
  */
class LazyValDetectionTests extends FunSuite with ExampleLoader {

  // ===== ExampleLoader implementation =====

  override val examplesDir: os.Path = os.pwd / "tests" / "src" / "test" / "resources" / "fixtures" / "examples"
  override val testWorkspace: os.Path = os.temp.dir(prefix = "sloth-tests-", deleteOnExit = true)
  override val quietCompilation: Boolean = true

  /** Scala versions to test with their expected lazy val implementations */
  val testVersions: Seq[(String, ScalaVersion)] = Seq(
    ("3.0.2", ScalaVersion.Scala30x_31x),
    ("3.1.3", ScalaVersion.Scala30x_31x),
    ("3.2.2", ScalaVersion.Scala32x),
    ("3.3.0", ScalaVersion.Scala33x_37x),
    ("3.3.6", ScalaVersion.Scala33x_37x),
    ("3.4.3", ScalaVersion.Scala33x_37x),
    ("3.5.2", ScalaVersion.Scala33x_37x),
    ("3.6.4", ScalaVersion.Scala33x_37x),
    ("3.7.3", ScalaVersion.Scala33x_37x),
    ("3.8.1", ScalaVersion.Scala38Plus)
  )

  override def requiredScalaVersions: Seq[String] = testVersions.map(_._1)

  val cleanupWorkspace: Boolean = true

  /** All discovered examples with their test data - loaded during class initialization */
  lazy val examples: Seq[LoadedExample] = {
    val fixturesDir = os.pwd / "tests" / "src" / "test" / "resources" / "fixtures"
    println(s"Fixtures directory: $fixturesDir")
    println(s"Examples directory: $examplesDir")
    println(s"Test workspace: $testWorkspace")

    println(s"Discovered ${discoveredExamples.size} examples: ${discoveredExamples.map(_.name).mkString(", ")}")

    val loaded = loadSelectedExamples()
    println(s"Loaded ${loaded.size} examples for testing")
    loaded
  }

  override def afterAll(): Unit = {
    // Clean up workspace
    if os.exists(testWorkspace) && cleanupWorkspace then
      println(s"Cleaning up test workspace: $testWorkspace")
      os.remove.all(testWorkspace)
  }

  /** Finds a compiled classfile for a specific example, Scala version, and class name.
    *
    * @param example
    *   The example test data
    * @param scalaVersion
    *   The Scala version
    * @param className
    *   The expected class name (e.g., "SimpleLazyVal$")
    * @return
    *   Path to the classfile if found
    */
  def findClassFile(example: LoadedExample, scalaVersion: String, className: String): Option[Path] = {
    example.compilationResult.results.get(scalaVersion).flatMap { versionResult =>
      versionResult.classFiles
        .find(cf => cf.relativePath == s"$className.class" || cf.relativePath.endsWith(s"/$className.class"))
        .map(_.absolutePath.toNIO)
    }
  }

  /** Generate tests for each example, class, and Scala version combination */
  def generateTests(): Unit = {
    examples.foreach { example =>
      example.metadata.expectedClasses.foreach { expectedClass =>
        testVersions.foreach { case (scalaVersion, expectedVersion) =>
          test(s"[${example.name}] ${expectedClass.className} in Scala $scalaVersion") {
            // Check if this version compiled successfully
            val wasCompiled = example.compilationResult.results.get(scalaVersion).exists(_.success)

            // Fail if compilation failed
            assert(wasCompiled, s"Scala $scalaVersion did not compile (likely JDK compatibility issue)")

            // Find the classfile
            val classFileOpt = findClassFile(example, scalaVersion, expectedClass.className)
            assert(
              classFileOpt.isDefined,
              s"Failed to find compiled classfile ${expectedClass.className}.class for Scala $scalaVersion"
            )

            val classFile = classFileOpt.get
            val bytes = Files.readAllBytes(classFile)

            // Parse classfile
            val parseResult = ClassfileParser.parse(bytes)
            assert(parseResult.isRight, s"Failed to parse classfile: ${parseResult.left.getOrElse("unknown error")}")

            val classInfo = parseResult.getOrElse(???)

            // Check for companion class (e.g., if testing Foo$, look for Foo)
            val companionClassInfo = if (expectedClass.className.endsWith("$")) {
              val companionClassName = expectedClass.className.stripSuffix("$")
              val companionClassFileOpt = findClassFile(example, scalaVersion, companionClassName)
              companionClassFileOpt.flatMap { companionClassFile =>
                val companionBytes = Files.readAllBytes(companionClassFile)
                ClassfileParser.parse(companionBytes).toOption
              }
            } else {
              None
            }

            // Detect lazy vals
            val detector = LazyValDetector()
            val detectionResult = detector.detect(classInfo, companionClassInfo)

            // Verify we found lazy vals
            detectionResult match {
              case LazyValDetectionResult.NoLazyVals =>
                if (expectedClass.lazyVals.isEmpty) {
                  // Expected no lazy vals - test passes
                  ()
                } else {
                  fail(s"Expected to find ${expectedClass.lazyVals.size} lazy val(s) but found none")
                }

              case LazyValDetectionResult.LazyValsFound(lazyVals, version) =>
                // Check expected count
                assertEquals(
                  lazyVals.size,
                  expectedClass.lazyVals.size,
                  s"Expected ${expectedClass.lazyVals.size} lazy val(s) but found ${lazyVals.size}"
                )

                // IMPORTANT: Tests should never detect Unknown version since we compile with known Scala 3 versions
                version match {
                  case ScalaVersion.Unknown(reason) =>
                    fail(s"Detected Unknown version for known Scala $scalaVersion - this indicates a bug in detection logic. Reason: $reason. LazyVals: ${lazyVals.map(lv => s"${lv.name} (version=${lv.version})").mkString(", ")}")
                  case _ => // OK
                }

                // Check version detection
                assertEquals(version, expectedVersion, s"Expected $expectedVersion but detected $version")

                // Verify each expected lazy val
                expectedClass.lazyVals.foreach { expectedLazyVal =>
                  val foundLazyVal = lazyVals.find(_.name == expectedLazyVal.name)
                  assert(
                    foundLazyVal.isDefined,
                    s"Expected to find lazy val '${expectedLazyVal.name}' but it was not detected"
                  )

                  val lazyVal = foundLazyVal.get
                  assertEquals(
                    lazyVal.index,
                    expectedLazyVal.index,
                    s"Expected lazy val '${expectedLazyVal.name}' to have index ${expectedLazyVal.index} but got ${lazyVal.index}"
                  )
                }

                // Additional version-specific checks
                expectedVersion match {
                  case ScalaVersion.Scala30x_31x | ScalaVersion.Scala32x =>
                    lazyVals.foreach { lazyVal =>
                      assert(lazyVal.bitmapField.isDefined, s"Expected bitmap field for ${lazyVal.name} in 3.0-3.2.x")
                      assert(
                        lazyVal.initMethod.isEmpty,
                        s"Did not expect lzyINIT method for ${lazyVal.name} in 3.0-3.2.x"
                      )
                    }

                  case ScalaVersion.Scala33x_37x =>
                    lazyVals.foreach { lazyVal =>
                      assert(
                        lazyVal.bitmapField.isEmpty,
                        s"Did not expect bitmap field for ${lazyVal.name} in 3.3-3.7.x"
                      )
                      assert(lazyVal.initMethod.isDefined, s"Expected lzyINIT method for ${lazyVal.name} in 3.3-3.7.x")
                      assert(lazyVal.offsetField.isDefined, s"Expected OFFSET field for ${lazyVal.name} in 3.3-3.7.x")
                      assertEquals(
                        lazyVal.storageField.descriptor,
                        "Ljava/lang/Object;",
                        s"Expected Object storage field for ${lazyVal.name} in 3.3-3.7.x"
                      )
                    }

                  case ScalaVersion.Scala38Plus =>
                    lazyVals.foreach { lazyVal =>
                      assert(lazyVal.bitmapField.isEmpty, s"Did not expect bitmap field for ${lazyVal.name} in 3.8+")
                      // OFFSET field is only present when there's a companion class split
                      // For standalone objects, VarHandle is used directly without OFFSET
                      assert(lazyVal.varHandleField.isDefined, s"Expected VarHandle field for ${lazyVal.name} in 3.8+")
                      assert(lazyVal.initMethod.isDefined, s"Expected lzyINIT method for ${lazyVal.name} in 3.8+")
                      assertEquals(
                        lazyVal.storageField.descriptor,
                        "Ljava/lang/Object;",
                        s"Expected Object storage field for ${lazyVal.name} in 3.8+"
                      )
                    }

                  case _ => ()
                }

              case LazyValDetectionResult.MixedVersions(lazyVals) =>
                val versionBreakdown = lazyVals.groupBy(_.version).map { case (v, lvs) => s"$v: ${lvs.map(_.name).mkString(", ")}" }.mkString("; ")
                fail(s"Unexpected mixed versions for known Scala $scalaVersion - this indicates a bug in detection logic. Breakdown: $versionBreakdown")
            }
          }
        }
      }
    }
  }

  // Generate all tests
  generateTests()

  test("Verify version classification logic") {
    // Test that version enums have correct properties
    assert(ScalaVersion.Scala30x_31x.isLegacy)
    assert(ScalaVersion.Scala32x.isLegacy)
    assert(ScalaVersion.Scala33x_37x.isLegacy)
    assert(!ScalaVersion.Scala38Plus.isLegacy)

    assert(ScalaVersion.Scala30x_31x.isBitmapBased)
    assert(ScalaVersion.Scala32x.isBitmapBased)
    assert(!ScalaVersion.Scala33x_37x.isBitmapBased)

    assert(ScalaVersion.Scala30x_31x.needsTransformation)
    assert(ScalaVersion.Scala32x.needsTransformation)
    assert(ScalaVersion.Scala33x_37x.needsTransformation)
    assert(!ScalaVersion.Scala38Plus.needsTransformation)
  }
}
