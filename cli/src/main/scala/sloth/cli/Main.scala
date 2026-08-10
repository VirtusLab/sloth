package sloth.cli

import sloth.analysis.{LazyValAnalyzer, ClassfileGroup}
import sloth.patching.BytecodePatcher
import sloth.jar.JarProcessor
import scala.util.{Try, Success, Failure}

/** CLI tool for patching Scala 3.x lazy val bytecode to 3.8+ format.
  *
  * Usage: sloth <directory>
  *
  * Processes all .class files in the directory (recursively), transforming Scala 3.3-3.7 lazy vals to 3.8+ format.
  */
object Main {

  /** Result of patching a classfile group */
  sealed trait PatchGroupResult
  object PatchGroupResult {
    case class Patched(filesPatched: Int) extends PatchGroupResult
    case object NotApplicable extends PatchGroupResult
    final case class Failed(error: String) extends PatchGroupResult
  }

  /** Summary of patching operations */
  case class PatchSummary(
      totalGroups: Int,
      totalFiles: Int,
      patchedGroups: Int,
      patchedFiles: Int,
      notApplicable: Int,
      failed: Int
  ) {
    def successful: Boolean = failed == 0
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      Console.err.println(fansi.Color.Red("Error: Expected exactly one argument (directory or JAR path)"))
      Console.err.println("Usage: sloth <directory|file.jar>")
      sys.exit(1)
    }

    val targetPath = os.Path(args(0), os.pwd)

    if (!os.exists(targetPath)) {
      Console.err.println(fansi.Color.Red(s"Error: Path does not exist: $targetPath"))
      sys.exit(1)
    }

    // Dispatch to JAR mode or directory mode
    if (targetPath.ext == "jar") {
      processJar(targetPath)
      return
    }

    val targetDir = targetPath

    if (!os.isDir(targetDir)) {
      Console.err.println(fansi.Color.Red(s"Error: Not a directory or JAR file: $targetDir"))
      sys.exit(1)
    }

    println(fansi.Bold.On("Sloth - Scala 3.x Lazy Val Bytecode Patcher"))
    println(fansi.Color.Cyan(s"Processing directory: $targetDir"))
    println()

    // Find and group classfiles
    val classFiles = findClassFiles(targetDir)

    if (classFiles.isEmpty) {
      println(fansi.Color.Yellow("No .class files found in directory"))
      sys.exit(0)
    }

    println(fansi.Color.Cyan(s"Found ${classFiles.size} classfile(s)"))
    println()

    // Read all classfiles into memory
    val classfileMap = classFiles.map { path =>
      val className = path.relativeTo(targetDir).toString.stripSuffix(".class").replace("/", ".")
      (className, os.read.bytes(path))
    }.toMap

    // Group classfiles (identify companion pairs)
    val groups = LazyValAnalyzer.group(classfileMap) match {
      case Right(g) => g
      case Left(error) =>
        Console.err.println(fansi.Color.Red(s"Error grouping classfiles: $error"))
        sys.exit(1)
    }

    println(fansi.Color.Cyan(s"Grouped into ${groups.size} classfile group(s)"))
    println()

    // Build a classloader that can resolve classes from the target directory
    val classLoader = new java.net.URLClassLoader(
      Array(targetDir.toNIO.toUri.toURL),
      getClass.getClassLoader
    )

    // Process each group
    val results = groups.map(processGroup(_, targetDir, classLoader))

    // Print summary
    println()
    println(fansi.Bold.On("=" * 80))
    printSummary(results)

    // Exit with appropriate code
    val summary = computeSummary(results)
    if (!summary.successful) {
      sys.exit(1)
    }
  }

  /** Processes a JAR file in-place: reads, patches, writes back */
  private def processJar(jarPath: os.Path): Unit = {
    println(fansi.Bold.On("Sloth - Scala 3.x Lazy Val Bytecode Patcher"))
    println(fansi.Color.Cyan(s"Processing JAR: $jarPath"))
    println()

    val input = jarPath.toNIO
    val tempOutput = os.temp(suffix = ".jar", deleteOnExit = true)

    val result = JarProcessor.process(input, tempOutput.toNIO)

    println(fansi.Bold.On("=" * 80))
    println(fansi.Bold.On("Summary:"))
    println(s"  Total classes: ${result.totalClasses}")
    println(fansi.Color.Green(s"  Patched: ${result.patchedClasses} class(es)"))
    println(fansi.Color.Blue(s"  Unchanged: ${result.totalClasses - result.patchedClasses - result.failedClasses}"))

    if (result.failedClasses > 0) {
      println(fansi.Color.Red(s"  Failed: ${result.failedClasses}"))
      for (error <- result.errors) {
        println(fansi.Color.Red(s"    - $error"))
      }
    }

    if (result.errors.isEmpty) {
      // Atomically replace original with patched version
      os.move(tempOutput, jarPath, replaceExisting = true)
      println()
      println(fansi.Color.Green(s"✓ JAR patched successfully!"))
    } else {
      println()
      println(fansi.Color.Red("Some classes failed to patch!"))
      // Still replace — partial patching is better than none
      os.move(tempOutput, jarPath, replaceExisting = true)
      sys.exit(1)
    }
  }

  /** Finds all .class files in a directory recursively */
  private def findClassFiles(dir: os.Path): Seq[os.Path] = {
    os.walk(dir)
      .filter(os.isFile)
      .filter(_.ext == "class")
      .toSeq
  }

  /** Processes a classfile group */
  private def processGroup(
      group: ClassfileGroup,
      targetDir: os.Path,
      classLoader: ClassLoader
  ): (String, PatchGroupResult) = {
    val groupName = group.primaryName
    print(fansi.Color.Cyan(s"Processing: $groupName ... "))

    Try {
      BytecodePatcher.patch(group, classLoader = Some(classLoader)) match {
        case BytecodePatcher.PatchResult.PatchedSingle(name, bytes) =>
          // Write back single file
          val filePath = targetDir / s"${name.replace('.', '/')}.class"
          os.write.over(filePath, bytes)
          PatchGroupResult.Patched(1)

        case BytecodePatcher.PatchResult.PatchedPair(
              companionObjectName,
              className,
              companionObjectBytes,
              classBytes
            ) =>
          // Write back both files
          val objectPath = targetDir / s"${companionObjectName.replace('.', '/')}.class"
          val classPath = targetDir / s"${className.replace('.', '/')}.class"
          os.write.over(objectPath, companionObjectBytes)
          os.write.over(classPath, classBytes)
          PatchGroupResult.Patched(2)

        case BytecodePatcher.PatchResult.NotApplicable =>
          PatchGroupResult.NotApplicable

        case BytecodePatcher.PatchResult.Failed(error) =>
          throw new RuntimeException(s"Patching failed: $error")
      }
    } match {
      case Success(result) =>
        result match {
          case PatchGroupResult.Patched(count) =>
            println(fansi.Color.Green(s"✓ PATCHED ($count file${if (count > 1) "s" else ""})"))
          case PatchGroupResult.NotApplicable =>
            println(fansi.Color.Blue("○ NOT APPLICABLE"))
          case PatchGroupResult.Failed(error) =>
            println(fansi.Color.Red(s"✗ FAILED: $error"))
        }
        (groupName, result)

      case Failure(exception) =>
        println(fansi.Color.Red(s"✗ FAILED: ${exception.getMessage}"))
        Console.err.println()
        Console.err.println(fansi.Color.Red("Error details:"))
        exception.printStackTrace(Console.err)
        Console.err.println()
        sys.exit(1)
    }
  }

  /** Computes summary statistics */
  private def computeSummary(results: Seq[(String, PatchGroupResult)]): PatchSummary = {
    val totalGroups = results.size
    val totalFiles = results
      .map(_._2 match {
        case PatchGroupResult.Patched(count) => count
        case _                               => 0
      })
      .sum + results.count(_._2 == PatchGroupResult.NotApplicable)

    val patchedGroups = results.count {
      case (_, PatchGroupResult.Patched(_)) => true
      case _                                => false
    }
    val patchedFiles = results.collect { case (_, PatchGroupResult.Patched(count)) =>
      count
    }.sum
    val notApplicable = results.count(_._2 == PatchGroupResult.NotApplicable)
    val failed = results.count {
      case (_, PatchGroupResult.Failed(_)) => true
      case _                               => false
    }

    PatchSummary(totalGroups, totalFiles, patchedGroups, patchedFiles, notApplicable, failed)
  }

  /** Prints summary of operations */
  private def printSummary(results: Seq[(String, PatchGroupResult)]): Unit = {
    val summary = computeSummary(results)

    println(fansi.Bold.On("Summary:"))
    println(s"  Total groups processed: ${summary.totalGroups}")
    println(fansi.Color.Green(s"  Patched: ${summary.patchedGroups} group(s), ${summary.patchedFiles} file(s)"))
    println(fansi.Color.Blue(s"  Not applicable: ${summary.notApplicable}"))

    if (summary.failed > 0) {
      println(fansi.Color.Red(s"  Failed: ${summary.failed}"))
      println()
      println(fansi.Color.Red("Some groups failed to patch!"))
    } else {
      println()
      println(fansi.Color.Green("✓ All groups processed successfully!"))
    }
  }
}
