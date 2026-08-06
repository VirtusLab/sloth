package sloth

import scala.collection.immutable.TreeSet

/** Trait that provides example discovery and loading functionality for tests.
  *
  * This trait manages a two-step process:
  *   1. Discovery: Load all example metadata without compilation 2. Loading: Compile specific examples as needed
  *
  * Use the SELECT_EXAMPLE environment variable to filter examples:
  *   - SELECT_EXAMPLE=simple-lazy-val (single example)
  *   - SELECT_EXAMPLE=simple-lazy-val,class-lazy-val (multiple examples)
  *   - (not set) - all examples available
  */
trait ExampleLoader {

  /** Information about a discovered example (metadata only, not compiled) */
  case class DiscoveredExample(
      name: String,
      path: os.Path,
      metadata: ExampleMetadata
  )

  /** Information about a loaded example (metadata + compiled bytecode) */
  case class LoadedExample(
      name: String,
      path: os.Path,
      metadata: ExampleMetadata,
      compilationResult: ExampleCompilationResult
  )

  // ===== Abstract members that must be provided by implementing classes =====

  /** The Scala versions needed for compilation (can be filtered by ONLY_SCALA_VERSIONS env var) */
  def requiredScalaVersions: Seq[String]

  /** The examples directory path */
  def examplesDir: os.Path

  /** The test workspace directory for compilation output */
  def testWorkspace: os.Path

  /** Whether to print detailed compilation output */
  def quietCompilation: Boolean = true

  /** Whether to suppress test output (printlns) - override in test suites to control verbosity */
  def quietTests: Boolean = true

  // ===== Discovery phase: Load metadata for all examples =====

  /** All discovered examples with their metadata (no compilation yet).
    *
    * This is always loaded regardless of SELECT_EXAMPLE filter, allowing tests to query what examples exist without
    * triggering compilation.
    */
  lazy val discoveredExamples: Seq[DiscoveredExample] = {
    val allExampleDirs = os.list(examplesDir).filter(os.isDir).toSeq

    allExampleDirs.flatMap { examplePath =>
      val exampleName = examplePath.last
      val metadataPath = examplePath / "metadata.json"

      // Skip directories without metadata.json (like 'tests' directory)
      if (!os.exists(metadataPath)) {
        println(s"Warning: Skipping example '$exampleName': No metadata.json found at $metadataPath")
        None
      } else {
        ExampleMetadata.load(examplePath) match {
          case Left(error) =>
            println(s"Warning: Failed to load metadata for example '$exampleName': $error")
            None

          case Right(metadata) =>
            Some(DiscoveredExample(exampleName, examplePath, metadata))
        }
      }
    }
  }

  /** Get the list of selected example names from the SELECT_EXAMPLE environment variable.
    *
    * @return
    *   Some(Set[String]) if SELECT_EXAMPLE is set, None if all examples should be included
    */
  lazy val selectedExamples: Option[Set[String]] = {
    sys.env.get("SELECT_EXAMPLE").map { value =>
      value.split(',').map(_.trim).filter(_.nonEmpty).toSet
    }
  }

  /** Get the list of selected Scala versions from the ONLY_SCALA_VERSIONS environment variable.
    *
    * @return
    *   Some(Set[String]) if ONLY_SCALA_VERSIONS is set, None if all versions should be included
    */
  lazy val selectedScalaVersions: Option[Set[String]] = {
    sys.env.get("ONLY_SCALA_VERSIONS").map { value =>
      value.split(',').map(_.trim).filter(_.nonEmpty).toSet
    }
  }

  /** Get whether bytecode inspection mode is enabled via INSPECT_BYTECODE env var.
    *
    * @return
    *   true if INSPECT_BYTECODE is set to "true", "1", or "yes" (case insensitive)
    */
  lazy val inspectBytecode: Boolean = {
    sys.env.get("INSPECT_BYTECODE").exists { value =>
      value.toLowerCase match {
        case "true" | "1" | "yes" => true
        case _                    => false
      }
    }
  }

  /** Check if an example is selected by the SELECT_EXAMPLE filter.
    *
    * @param exampleName
    *   The name of the example
    * @return
    *   true if the example should be included (either no filter or explicitly selected)
    */
  def isExampleSelected(exampleName: String): Boolean = {
    selectedExamples match {
      case None           => true // No filter - all examples selected
      case Some(selected) => selected.contains(exampleName)
    }
  }

  /** Check if a Scala version is selected by the ONLY_SCALA_VERSIONS filter.
    *
    * @param scalaVersion
    *   The Scala version to check
    * @return
    *   true if the version should be included (either no filter or explicitly selected)
    */
  def isScalaVersionSelected(scalaVersion: String): Boolean = {
    selectedScalaVersions match {
      case None           => true // No filter - all versions selected
      case Some(selected) => selected.contains(scalaVersion)
    }
  }

  /** Get the filtered list of Scala versions based on ONLY_SCALA_VERSIONS env var.
    *
    * @return
    *   Filtered list of Scala versions to use for testing
    */
  def filteredScalaVersions: Seq[String] = {
    selectedScalaVersions match {
      case None => requiredScalaVersions
      case Some(selected) =>
        val filtered = requiredScalaVersions.filter(selected.contains)
        if (filtered.isEmpty) {
          println(
            s"Warning: ONLY_SCALA_VERSIONS filter matched no versions. " +
              s"Selected: ${selected.mkString(", ")}. Available: ${requiredScalaVersions.mkString(", ")}"
          )
        }
        filtered
    }
  }

  // ===== Loading phase: Compile examples as needed =====

  /** Create an ExampleRunner for compiling examples */
  private lazy val runner = ExampleRunner(examplesDir, testWorkspace, quiet = quietCompilation)

  /** Load examples matching a predicate, compiling them with the required Scala versions.
    *
    * @param predicate
    *   Filter function to select which discovered examples to load
    * @return
    *   Loaded examples with compilation results
    */
  def loadExamples(predicate: DiscoveredExample => Boolean): Seq[LoadedExample] = {
    val toLoad = discoveredExamples.filter(predicate)

    if (toLoad.isEmpty) {
      println("Warning: No examples match the provided predicate")
      return Seq.empty
    }

    val versions = filteredScalaVersions.to(TreeSet)
    println(s"Loading ${toLoad.size} example(s) with Scala versions: ${versions.mkString(", ")}")

    toLoad.flatMap { discovered =>
      runner.compileExample(discovered.path, versions) match {
        case Right(compilationResult) =>
          val successful = compilationResult.successfulResults.size
          val total = compilationResult.results.size
          println(s"Example '${discovered.name}': $successful/$total versions compiled")
          Some(LoadedExample(discovered.name, discovered.path, discovered.metadata, compilationResult))

        case Left(error) =>
          println(s"Warning: Failed to compile example '${discovered.name}': $error")
          None
      }
    }
  }

  /** Load a single example by name, compiling it with the required Scala versions.
    *
    * @param exampleName
    *   The name of the example to load
    * @return
    *   The loaded example, or None if not found or compilation failed
    */
  def loadExample(exampleName: String): Option[LoadedExample] = {
    val results = loadExamples(_.name == exampleName)
    results.headOption
  }

  /** Load examples selected by the SELECT_EXAMPLE environment variable.
    *
    * This is the default loading method that respects the filter. If no filter is set, loads all examples. If a filter
    * is set, loads only the selected examples.
    *
    * @return
    *   Loaded examples matching the SELECT_EXAMPLE filter
    */
  def loadSelectedExamples(): Seq[LoadedExample] = {
    selectedExamples match {
      case None =>
        // No filter - load all discovered examples
        loadExamples(_ => true)

      case Some(selected) =>
        // Load only selected examples
        val results = loadExamples(discovered => selected.contains(discovered.name))

        // Warn if any selected examples were not found
        val foundNames = results.map(_.name).toSet
        val notFound = selected.diff(foundNames)
        if (notFound.nonEmpty) {
          println(s"Warning: Selected examples not found or failed to compile: ${notFound.mkString(", ")}")
        }

        if (results.isEmpty) {
          throw new RuntimeException(
            s"No examples matched SELECT_EXAMPLE filter: ${selected.mkString(", ")}\n" +
              s"Available examples: ${discoveredExamples.map(_.name).mkString(", ")}"
          )
        }

        println(s"Filtered to ${results.size} example(s): ${results.map(_.name).mkString(", ")}")
        results
    }
  }

  // ===== Bytecode inspection utilities =====

  /** Extract javap output for a specific class file.
    *
    * @param classFile
    *   The ClassFileInfo containing the absolute path to the .class file
    * @return
    *   Either error message or javap output string
    */
  def extractJavap(classFile: ClassFileInfo): Either[String, String] = {
    import scala.sys.process._

    try {
      val result = os
        .proc("javap", "-v", "-p", classFile.absolutePath.toString)
        .call(check = false)

      if (result.exitCode == 0) {
        Right(result.out.text())
      } else {
        Left(s"javap failed with exit code ${result.exitCode}: ${result.err.text()}")
      }
    } catch {
      case e: Exception =>
        Left(s"Failed to run javap: ${e.getMessage}")
    }
  }

  /** Print bytecode inspection report for a specific class across versions.
    *
    * @param loadedExample
    *   The loaded example containing compilation results
    * @param className
    *   The class name (e.g., "Foo$" or "Foo")
    * @param scalaVersions
    *   Optional sequence of specific versions to inspect (defaults to all successful versions)
    */
  def printBytecodeInspection(
      loadedExample: LoadedExample,
      className: String,
      scalaVersions: Option[Seq[String]] = None
  ): Unit = {
    val versionsToInspect = scalaVersions.getOrElse(
      loadedExample.compilationResult.successfulResults.keys.toSeq.sorted
    )

    println("\n" + "=" * 80)
    println(s"BYTECODE INSPECTION: ${loadedExample.name} / $className")
    println("=" * 80)

    val classFilePath = s"$className.class"

    versionsToInspect.foreach { version =>
      loadedExample.compilationResult.classFile(classFilePath, version) match {
        case Some(classFile) =>
          println(s"\n--- Scala $version ---")
          println(s"Path: ${classFile.absolutePath}")
          println(s"Size: ${classFile.size} bytes")
          println(s"SHA256: ${classFile.sha256}")
          println()

          extractJavap(classFile) match {
            case Right(javapOutput) =>
              println(javapOutput)
            case Left(error) =>
              println(s"ERROR: $error")
          }

        case None =>
          println(s"\n--- Scala $version ---")
          println(s"ERROR: Class file not found: $classFilePath")
      }
    }

    println("\n" + "=" * 80 + "\n")
  }

  /** Print bytecode inspection report for all classes in an example.
    *
    * @param loadedExample
    *   The loaded example containing compilation results
    * @param scalaVersions
    *   Optional sequence of specific versions to inspect (defaults to all successful versions)
    */
  def printAllBytecodeInspection(
      loadedExample: LoadedExample,
      scalaVersions: Option[Seq[String]] = None
  ): Unit = {
    val allClassPaths = loadedExample.compilationResult.allClassPaths.toSeq.sorted

    allClassPaths.foreach { classPath =>
      val className = classPath.stripSuffix(".class")
      printBytecodeInspection(loadedExample, className, scalaVersions)
    }
  }

  /** Inspect bytecode on test failure (call this from test assertions).
    *
    * @param loadedExample
    *   The loaded example containing compilation results
    * @param failedScalaVersion
    *   The Scala version that failed the test
    * @param failureContext
    *   Description of what failed (e.g., "lazy val detection", "patching")
    */
  def inspectOnFailure(
      loadedExample: LoadedExample,
      failedScalaVersion: String,
      failureContext: String
  ): Unit = {
    if (inspectBytecode) {
      println(s"\n!!! TEST FAILURE: $failureContext !!!")
      println(s"Example: ${loadedExample.name}, Scala version: $failedScalaVersion\n")

      // Print bytecode for the failed version and adjacent versions for comparison
      val allVersions = loadedExample.compilationResult.successfulResults.keys.toSeq.sorted
      val versionIdx = allVersions.indexOf(failedScalaVersion)

      val versionsToShow = if (versionIdx >= 0) {
        // Show previous, current, and next versions
        val start = Math.max(0, versionIdx - 1)
        val end = Math.min(allVersions.size, versionIdx + 2)
        allVersions.slice(start, end)
      } else {
        // Version not found, just show the failed version
        Seq(failedScalaVersion)
      }

      println(s"Inspecting versions: ${versionsToShow.mkString(", ")}\n")

      printAllBytecodeInspection(loadedExample, Some(versionsToShow))
    }
  }
}
