package sloth

/** Information about a single compiled class file */
case class ClassFileInfo(
    relativePath: String,
    absolutePath: os.Path,
    size: Long,
    sha256: String
)

/** Result of compiling an example with a specific Scala version */
case class VersionCompilationResult(
    scalaVersion: String,
    success: Boolean,
    classFiles: Set[ClassFileInfo],
    error: Option[String] = None
)

/** Complete result of compiling an example across multiple Scala versions */
case class ExampleCompilationResult(
    exampleName: String,
    results: Map[String, VersionCompilationResult]
) {

  /** Get all unique class file relative paths across all versions */
  def allClassPaths: Set[String] =
    results.values.flatMap(_.classFiles.map(_.relativePath)).toSet

  /** Get class file info for a specific path and version */
  def classFile(relativePath: String, version: String): Option[ClassFileInfo] =
    results.get(version).flatMap(_.classFiles.find(_.relativePath == relativePath))

  /** Group class files by relative path across versions */
  def classFilesByPath: Map[String, Map[String, ClassFileInfo]] =
    allClassPaths.map { path =>
      val filesForPath = results
        .flatMap { case (version, result) =>
          result.classFiles
            .find(_.relativePath == path)
            .map(version -> _)
        }
      path -> filesForPath
    }.toMap

  /** Check if all versions produced identical bytecode for a given class */
  def isIdentical(relativePath: String): Boolean = {
    val shas = classFilesByPath
      .get(relativePath)
      .map(_.values.map(_.sha256).toSet)
      .getOrElse(Set.empty)
    shas.size <= 1
  }

  /** Get all class paths where bytecode differs between versions */
  def driftingClasses: Set[String] =
    allClassPaths.filter(!isIdentical(_))

  /** Get successful compilations only */
  def successfulResults: Map[String, VersionCompilationResult] =
    results.filter(_._2.success)
}
