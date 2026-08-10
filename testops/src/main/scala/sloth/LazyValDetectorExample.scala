package sloth

import sloth.classfile.ClassfileParser
import sloth.lazyval.{LazyValDetector, LazyValComparator, LazyValFormatter}
import scribe.{info, error}
import java.nio.file.{Files, Paths}
import scala.util.{Try, Success, Failure}

/** Example program demonstrating lazy val detection and comparison. */
@main def detectLazyVals(classfilePath: String): Unit =
  info(s"Analyzing classfile: $classfilePath")

  val result = for
    bytes <- Try(Files.readAllBytes(Paths.get(classfilePath)))
    classInfo <- ClassfileParser().parseClassfileTry(bytes)
  yield
    info(s"Successfully parsed class: ${classInfo.name}")

    // Detect lazy vals
    val detector = LazyValDetector()
    val detectionResult = detector.detect(classInfo)

    println("\n" + "=" * 80)
    println("LAZY VAL DETECTION RESULT")
    println("=" * 80)
    println(LazyValFormatter.formatDetectionResult(detectionResult))

  result match
    case Success(_) =>
      info("Analysis completed successfully")
    case Failure(ex) =>
      error("Analysis failed", ex)
      System.exit(1)

/** Compare lazy vals between two classfiles. */
@main def compareLazyVals(classfilePath1: String, classfilePath2: String): Unit =
  info(s"Comparing classfiles:")
  info(s"  First:  $classfilePath1")
  info(s"  Second: $classfilePath2")

  val result = for
    bytes1 <- Try(Files.readAllBytes(Paths.get(classfilePath1)))
    bytes2 <- Try(Files.readAllBytes(Paths.get(classfilePath2)))
    class1 <- ClassfileParser().parseClassfileTry(bytes1)
    class2 <- ClassfileParser().parseClassfileTry(bytes2)
  yield
    info(s"Parsed: ${class1.name} vs ${class2.name}")

    // Compare lazy vals
    val comparator = LazyValComparator()
    val comparisonResult = comparator.compare(class1, class2)

    println("\n" + "=" * 80)
    println("LAZY VAL COMPARISON RESULT")
    println("=" * 80)
    println(LazyValFormatter.formatComparisonResult(comparisonResult))

  result match
    case Success(_) =>
      info("Comparison completed successfully")
    case Failure(ex) =>
      error("Comparison failed", ex)
      System.exit(1)

/** Analyze all classfiles in a directory. */
@main def analyzeDirectory(dirPath: String): Unit =
  info(s"Analyzing all classfiles in: $dirPath")

  val dir = Paths.get(dirPath)
  if !Files.exists(dir) || !Files.isDirectory(dir) then
    error(s"Directory does not exist: $dirPath")
    System.exit(1)

  import scala.jdk.StreamConverters._

  val classfiles = Files
    .walk(dir)
    .toScala(LazyList)
    .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".class"))
    .toSeq

  info(s"Found ${classfiles.size} classfiles")

  val results = classfiles.flatMap { path =>
    Try {
      val bytes = Files.readAllBytes(path)
      val classInfo = ClassfileParser().parseClassfileTry(bytes).get
      val detectionResult = LazyValDetector.detect(classInfo)
      (path.getFileName.toString, classInfo.name, detectionResult)
    }.toOption
  }

  println("\n" + "=" * 80)
  println(s"LAZY VAL ANALYSIS SUMMARY (${results.size} classes)")
  println("=" * 80)

  val withLazyVals = results.filter { case (_, _, result) =>
    result match
      case sloth.lazyval.LazyValDetectionResult.NoLazyVals => false
      case _                                               => true
  }

  if withLazyVals.isEmpty then println("No lazy vals found in any classfile")
  else
    println(s"\nFound lazy vals in ${withLazyVals.size} classes:\n")
    withLazyVals.foreach { case (filename, className, result) =>
      println(s"$filename ($className):")
      println(s"  ${LazyValFormatter.formatSummary(result)}")
      println()
    }

  info("Analysis completed")
