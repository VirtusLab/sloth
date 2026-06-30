package lazyvalgrade

import java.security.MessageDigest
import scala.util.{Try, Success, Failure, boundary}
import scala.util.boundary.break
import scala.concurrent.{Future, ExecutionContext, blocking, Await, duration}
import ExecutionContext.Implicits.global
import duration._
import lazyvalgrade.analysis.{LazyValAnalyzer, ClassfileGroup}
import lazyvalgrade.lazyval.{LazyValDetector, LazyValDetectionResult, ScalaVersion}
import lazyvalgrade.patching.BytecodePatcher

/** Single chokepoint that boots the scala-cli Bloop compilation server exactly once per JVM.
  *
  * The fixtures compile each Scala version in parallel. Starting the Bloop daemon is the only step
  * that races: many scala-cli clients hitting `ensureBloopRunning` at once on a cold CI runner all
  * try to spawn the server and time out (`TimeoutException: Future timed out after [30 seconds]`).
  * Once the daemon is up, every client just connects to it — so we warm it once, behind a `lazy val`
  * (which the JVM initializes under a lock, blocking concurrent callers until it completes), and only
  * then let the parallel compilations proceed. Keeping Bloop (vs `--server=false`) preserves its
  * large incremental-compile speedup.
  */
object BloopWarmup {
  // First access boots the daemon; concurrent accessors block on lazy-val init until it's ready.
  private lazy val booted: Unit = {
    val dir = os.temp.dir(prefix = "lazyvalgrade-bloop-warmup-")
    os.write.over(dir / "Warmup.scala", "object Warmup\n")
    // Generous startup timeout so a cold daemon (JVM download + start) doesn't trip the 30s default.
    os.proc("scala-cli", "compile", "--jvm", "17", "--bloop-startup-timeout", "180s", "-S", "3.3.8", dir.toString)
      .call(check = false, stderr = os.Pipe, stdout = os.Pipe)
    os.remove.all(dir)
  }

  /** Ensure the Bloop daemon is running before launching parallel compilations. Idempotent. */
  def ensure(): Unit = booted
}

/** Runs Scala compilation examples across multiple versions
  *
  * @param examplesRoot
  *   Root directory containing example subdirectories
  * @param workspaceRoot
  *   Root directory for compilation output
  * @param quiet
  *   If true, suppress all logging except errors and only show scala-cli output on failure
  */
class ExampleRunner(
    examplesRoot: os.Path,
    workspaceRoot: os.Path,
    quiet: Boolean = false
) {
  import scribe._

  // Set scribe logging level based on quiet mode
  if (quiet) {
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(minimumLevel = Some(Level.Error))
      .replace()
  }

  /** Validates that a directory exists and is a directory */
  private def validateDirectory(path: os.Path, name: String): Either[String, os.Path] =
    if !os.exists(path) then Left(s"$name does not exist: $path")
    else if !os.isDir(path) then Left(s"$name is not a directory: $path")
    else Right(path)

  /** Validates that a directory contains Scala source files */
  private def validateScalaSource(dir: os.Path): Either[String, os.Path] =
    val hasScalaFiles = os
      .list(dir)
      .exists(p => os.isFile(p) && p.toString.endsWith(".scala"))

    if hasScalaFiles then Right(dir)
    else Left(s"Directory contains no .scala files: $dir")

  /** Computes SHA-256 hash of a byte array */
  private def sha256(bytes: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    digest.digest(bytes).map("%02x".format(_)).mkString
  }

  /** Compiles an example directory with a specific Scala version
    *
    * @param sourceDir
    *   Directory containing Scala source files
    * @param scalaVersion
    *   Scala version to compile with
    * @param exampleWorkspace
    *   Workspace directory for this example
    * @return
    *   Try containing the compilation result
    */
  private def compileWithScalaCli(
      sourceDir: os.Path,
      scalaVersion: String,
      exampleWorkspace: os.Path
  ): Try[VersionCompilationResult] = Try {
    val targetDir = exampleWorkspace / scalaVersion
    os.makeDir.all(targetDir)

    // Copy sources to target directory
    os.walk(sourceDir)
      .filter(os.isFile)
      .foreach { source =>
        val relative = source.relativeTo(sourceDir)
        val target = targetDir / relative
        os.makeDir.all(target / os.up)
        os.copy.over(source, target)
      }

    if (!quiet) {
      info(s"Compiling with Scala $scalaVersion...")
    }

    // Run scala-cli compile on JDK 17 (the compiler runtime — needed by both old and new Scala
    // versions). Target Java 9 bytecode (--release 9) so fixtures load on a Java 9 JVM, which the
    // tests-jdk9 runtime suite requires. This only lowers the classfile version; the lazy-val
    // scheme (bitmap / Unsafe) is unchanged. Scala 3.8.x cannot emit < v61, so it keeps its
    // default target and is therefore only run on the JDK-25 leg (used as a static reference here).
    val releaseArgs = if (scalaVersion.startsWith("3.8")) Seq.empty else Seq("--release", "9")
    val result = os
      .proc("scala-cli", "compile", "--jvm", "17", "--bloop-startup-timeout", "180s", releaseArgs, "-S", scalaVersion, targetDir.toString)
      .call(cwd = targetDir, stderr = os.Pipe, stdout = os.Pipe, check = false)

    // Only log output if compilation failed or not in quiet mode
    if (result.exitCode != 0) {
      val stdout = result.out.text()
      val stderr = result.err.text()
      error(s"Compilation failed for Scala $scalaVersion (exit code ${result.exitCode})")
      if (stdout.nonEmpty) {
        error(s"stdout:\n$stdout")
      }
      if (stderr.nonEmpty) {
        error(s"stderr:\n$stderr")
      }
      throw new Exception(s"Compilation failed with exit code ${result.exitCode}")
    } else if (!quiet) {
      val output = result.out.text()
      debug(s"Compilation output for $scalaVersion: $output")
    }

    // Collect class files
    val classFiles = collectClassFiles(targetDir)

    VersionCompilationResult(
      scalaVersion = scalaVersion,
      success = true,
      classFiles = classFiles
    )
  }.recoverWith { case e: Exception =>
    error(s"Compilation failed for Scala $scalaVersion: ${e.getMessage}")
    Success(
      VersionCompilationResult(
        scalaVersion = scalaVersion,
        success = false,
        classFiles = Set.empty,
        error = Some(e.getMessage)
      )
    )
  }

  /** Collects class files from a compiled directory
    *
    * @param compiledDir
    *   Directory containing .scala-build output
    * @return
    *   Set of ClassFileInfo for all found class files
    */
  private def collectClassFiles(compiledDir: os.Path): Set[ClassFileInfo] = {
    val scalaBuildDir = compiledDir / ".scala-build"

    if !os.exists(scalaBuildDir) then
      if (!quiet) {
        warn(s".scala-build directory not found in $compiledDir")
      }
      return Set.empty

    // Find all .class files, skip .bloop directories
    val classFiles = os
      .walk(scalaBuildDir)
      .filter(p => os.isFile(p) && p.toString.endsWith(".class"))
      .filter(p => !p.toString.contains(".bloop"))
      .filter(p => !p.last.contains("$package"))

    classFiles.flatMap { classFile =>
      Try {
        val bytes = os.read.bytes(classFile)
        val fullPath = classFile.relativeTo(scalaBuildDir).toString

        // Normalize path: extract only the part after classes/main/ or classes/test/
        val normalizedPath = fullPath.split("/").toSeq match {
          case parts if parts.contains("classes") =>
            val classesIdx = parts.indexOf("classes")
            if classesIdx + 2 < parts.size then Some(parts.drop(classesIdx + 2).mkString("/"))
            else None
          case _ => None
        }

        normalizedPath.map { path =>
          ClassFileInfo(
            relativePath = path,
            absolutePath = classFile,
            size = bytes.length,
            sha256 = sha256(bytes)
          )
        }
      }.toOption.flatten
    }.toSet
  }

  /** Runs compilation for an example across multiple Scala versions
    *
    * @param examplePath
    *   Path to the example directory (relative to examplesRoot or absolute)
    * @param scalaVersions
    *   Set of Scala versions to compile with
    * @return
    *   Either error message or compilation results
    */
  def compileExample(
      examplePath: os.Path,
      scalaVersions: Set[String]
  ): Either[String, ExampleCompilationResult] = {
    // Resolve example path (handle both absolute and relative)
    val resolvedExample =
      if examplePath.startsWith(os.root) then examplePath
      else examplesRoot / os.RelPath(examplePath.toString)

    if (!quiet) {
      info(s"Running example: ${resolvedExample.last}")
    }

    // Clean desk protocol: nuke all .scala-build and .bsp directories from source
    // BEFORE any parallel compilation starts to avoid race conditions
    Seq(".scala-build", ".bsp").foreach { dirName =>
      val buildDir = resolvedExample / dirName
      if (os.exists(buildDir)) {
        if (!quiet) {
          debug(s"Cleaning $dirName from ${resolvedExample.last}...")
        }
        os.remove.all(buildDir)
      }
    }

    for {
      _ <- validateDirectory(resolvedExample, "Example directory")
      _ <- validateScalaSource(resolvedExample)
      _ <- validateDirectory(workspaceRoot, "Workspace directory")
    } yield {
      val exampleName = resolvedExample.last
      val exampleWorkspace = workspaceRoot / exampleName
      os.makeDir.all(exampleWorkspace)

      // Boot the Bloop daemon once (synchronously) before the parallel compiles below, so they all
      // connect to a running server instead of racing to start it.
      BloopWarmup.ensure()

      val results = Future
        .sequence(scalaVersions.toSeq.map { version =>
          Future {
            blocking {
              val result = compileWithScalaCli(resolvedExample, version, exampleWorkspace)
              if (!quiet) {
                result match {
                  case Success(r) if r.success =>
                    info(
                      s"  ✓ Scala $version compiled successfully (${r.classFiles.size} class files)"
                    )
                  case Success(r) =>
                    warn(s"  ✗ Scala $version compilation failed")
                  case Failure(e) =>
                    error(s"  ✗ Scala $version failed with exception: ${e.getMessage}")
                }
              }
              version -> result.get
            }
          }
        })
        .map(_.toMap)

      ExampleCompilationResult(
        exampleName = exampleName,
        results = Await.result(results, 3.minutes) // TODO: Make this configurable
      )
    }
  }

  /** Runs compilation for multiple examples
    *
    * @param examplePaths
    *   Paths to example directories
    * @param scalaVersions
    *   Set of Scala versions to compile with
    * @return
    *   Sequence of results or errors
    */
  def compileExamples(
      examplePaths: Seq[os.Path],
      scalaVersions: Set[String]
  ): Seq[Either[String, ExampleCompilationResult]] =
    examplePaths.map(compileExample(_, scalaVersions))

  /** Patches compiled classfiles for a specific version
    *
    * @param compiledDir
    *   Directory containing compiled classfiles
    * @param patchedDir
    *   Directory to write patched classfiles
    * @param scalaVersion
    *   Scala version being patched
    * @return
    *   Either error message or map of class names to patched file paths
    */
  def patchCompiledVersion(
      compiledDir: os.Path,
      patchedDir: os.Path,
      scalaVersion: String
  ): Either[String, Map[String, Seq[os.Path]]] = {
    // Collect class files
    val classFiles = collectClassFiles(compiledDir)

    if (classFiles.isEmpty) {
      return Left(s"No class files found in $compiledDir")
    }

    // Build classfile map (className -> bytes)
    val classfileMap = classFiles.map { cf =>
      val bytes = os.read.bytes(cf.absolutePath)
      val className = cf.relativePath.stripSuffix(".class").replace("/", ".")
      (className, bytes)
    }.toMap

    // Build a classloader that can resolve classes from the compiled directory
    val compiledClassLoader = new java.net.URLClassLoader(
      Array(compiledDir.toNIO.toUri.toURL),
      getClass.getClassLoader
    )

    // Group classfiles (detect companion pairs)
    LazyValAnalyzer.group(classfileMap).flatMap { groups =>
      // Validate that we can detect versions for all groups
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
              break(Left(s"Detected Unknown version for ${group.primaryName} compiled with Scala $scalaVersion: $reason. LazyVals: ${lazyVals.map(lv => s"${lv.name} (version=${lv.version})").mkString(", ")}"))
            case LazyValDetectionResult.LazyValsFound(_, _) => // OK
            case LazyValDetectionResult.MixedVersions(lazyVals) =>
              val versionBreakdown = lazyVals.groupBy(_.version).map { case (v, lvs) => s"$v: ${lvs.map(_.name).mkString(", ")}" }.mkString("; ")
              break(Left(s"Detected mixed versions for ${group.primaryName} compiled with Scala $scalaVersion. Breakdown: $versionBreakdown"))
          }
        }

        // Patch each group
        val resultMap = scala.collection.mutable.Map[String, Seq[os.Path]]()
        var error: Option[String] = None

        groups.foreach { group =>
          if (error.isEmpty) {
            BytecodePatcher.patch(group, classLoader = Some(compiledClassLoader)) match {
              case BytecodePatcher.PatchResult.PatchedSingle(name, bytes) =>
                // Write single patched file
                val patchedPath = writePatchedFile(name, bytes, patchedDir)
                resultMap(name) = Seq(patchedPath)

              case BytecodePatcher.PatchResult
                    .PatchedPair(companionObjectName, className, companionObjectBytes, classBytes) =>
                // Write both patched files
                val objectPath = writePatchedFile(companionObjectName, companionObjectBytes, patchedDir)
                val classPath = writePatchedFile(className, classBytes, patchedDir)
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

  /** Writes a patched classfile to the patched directory. */
  private def writePatchedFile(className: String, bytes: Array[Byte], patchedDir: os.Path): os.Path = {
    val relativePath = className.replace('.', '/') + ".class"
    val patchedPath = patchedDir / os.RelPath(relativePath)

    os.makeDir.all(patchedPath / os.up)
    os.write.over(patchedPath, bytes)

    patchedPath
  }

  /** Patches all versions for an example
    *
    * @param exampleName
    *   Name of the example
    * @param exampleWorkspace
    *   Workspace containing compiled versions
    * @param scalaVersions
    *   Set of Scala versions to patch
    * @return
    *   Either error message or success message
    */
  def patchExample(
      exampleName: String,
      exampleWorkspace: os.Path,
      scalaVersions: Set[String]
  ): Either[String, String] = {
    val patchedRoot = exampleWorkspace / "patched"

    // Only patch versions 3.0-3.7 (3.8+ already uses VarHandle natively)
    val patchableVersions = scalaVersions.filter { version =>
      version.startsWith("3.0") || version.startsWith("3.1") ||
      version.startsWith("3.2") || version.startsWith("3.3") ||
      version.startsWith("3.4") || version.startsWith("3.5") ||
      version.startsWith("3.6") || version.startsWith("3.7")
    }

    if (patchableVersions.isEmpty) {
      if (!quiet) {
        info(s"No patchable versions found for $exampleName (need 3.0-3.7)")
      }
      return Right(s"No patchable versions for $exampleName")
    }

    var successCount = 0
    var failCount = 0
    val errors = scala.collection.mutable.ListBuffer[String]()

    patchableVersions.foreach { version =>
      val compiledDir = exampleWorkspace / version
      val patchedDir = patchedRoot / version

      if (!quiet) {
        info(s"Patching $exampleName/$version...")
      }

      patchCompiledVersion(compiledDir, patchedDir, version) match {
        case Right(patchedFiles) =>
          successCount += 1
          if (!quiet) {
            info(s"  ✓ Patched successfully (${patchedFiles.size} classes)")
          }
        case Left(err) =>
          if (err.contains("No class files found") || err.contains("NotApplicable")) {
            // Expected for examples without lazy vals
            if (!quiet) {
              debug(s"  - Skipped: $err")
            }
          } else {
            failCount += 1
            val errorMsg = s"  ✗ Failed: $err"
            warn(errorMsg)
            errors += errorMsg
          }
      }
    }

    if (failCount > 0) {
      Left(s"Patching completed with errors: $successCount succeeded, $failCount failed. Errors:\n${errors.mkString("\n")}")
    } else if (successCount > 0) {
      Right(s"Patched $successCount versions successfully")
    } else {
      Right(s"No versions were patched (no lazy vals or not applicable)")
    }
  }

  /** Discovers all example directories in the examples root
    *
    * @return
    *   Either error message or sequence of example paths
    */
  def discoverExamples(): Either[String, Seq[os.Path]] =
    validateDirectory(examplesRoot, "Examples root").map { root =>
      os.list(root)
        .filter(os.isDir)
        .filter { dir =>
          os.list(dir)
            .exists(p => os.isFile(p) && p.toString.endsWith(".scala"))
        }
        .toSeq
    }
}

object ExampleRunner {

  /** Creates an ExampleRunner with os.Path parameters */
  def apply(examplesRoot: os.Path, workspaceRoot: os.Path, quiet: Boolean = false): ExampleRunner =
    new ExampleRunner(examplesRoot, workspaceRoot, quiet)

  /** Creates an ExampleRunner with String paths */
  def apply(examplesRoot: String, workspaceRoot: String, quiet: Boolean): ExampleRunner =
    new ExampleRunner(os.Path(examplesRoot), os.Path(workspaceRoot), quiet)
}
