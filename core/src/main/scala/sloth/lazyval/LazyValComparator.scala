package sloth.lazyval

import sloth.classfile.{ClassInfo, FieldInfo, MethodInfo}

/** Result of comparing lazy val implementations between two classes. */
sealed trait LazyValComparisonResult

object LazyValComparisonResult:
  /** Both classes have no lazy vals */
  case object BothNoLazyVals extends LazyValComparisonResult

  /** Only one class has lazy vals */
  final case class OnlyOneHasLazyVals(
      firstHas: Boolean,
      lazyVals: Seq[LazyValInfo]
  ) extends LazyValComparisonResult

  /** Both have lazy vals with same semantic implementation (same version) */
  final case class SameImplementation(
      version: ScalaVersion,
      lazyVals1: Seq[LazyValInfo],
      lazyVals2: Seq[LazyValInfo]
  ) extends LazyValComparisonResult

  /** Both have lazy vals but with different implementation versions */
  final case class DifferentImplementations(
      version1: ScalaVersion,
      version2: ScalaVersion,
      lazyVals1: Seq[LazyValInfo],
      lazyVals2: Seq[LazyValInfo],
      differences: Seq[LazyValDifference]
  ) extends LazyValComparisonResult

/** Specific difference in lazy val implementation. */
sealed trait LazyValDifference:
  def description: String

object LazyValDifference:
  /** A lazy val is only present in one class */
  final case class OnlyInOne(
      name: String,
      index: Int,
      inFirst: Boolean
  ) extends LazyValDifference:
    def description: String =
      s"Lazy val $name (index $index) only in ${if inFirst then "first" else "second"} class"

  /** Field structure differs for a lazy val */
  final case class FieldStructureDiffers(
      name: String,
      index: Int,
      details: String
  ) extends LazyValDifference:
    def description: String =
      s"Lazy val $name (index $index) has different field structure: $details"

  /** Method structure differs for a lazy val */
  final case class MethodStructureDiffers(
      name: String,
      index: Int,
      details: String
  ) extends LazyValDifference:
    def description: String =
      s"Lazy val $name (index $index) has different method structure: $details"

  /** Implementation version differs */
  final case class VersionDiffers(
      name: String,
      index: Int,
      version1: ScalaVersion,
      version2: ScalaVersion
  ) extends LazyValDifference:
    def description: String =
      s"Lazy val $name (index $index) differs: $version1 vs $version2"

/** Compares lazy val implementations between two classes.
  *
  * Focuses specifically on lazy val implementation patterns, ignoring other bytecode differences.
  */
final class LazyValComparator:
  import scribe.{info, debug}

  /** Compares lazy val implementations in two classes.
    *
    * @param class1
    *   First class
    * @param class2
    *   Second class
    * @return
    *   Comparison result focusing on lazy vals
    */
  def compare(
      class1: ClassInfo,
      class2: ClassInfo
  ): LazyValComparisonResult =
    debug(s"Comparing lazy vals in ${class1.name} vs ${class2.name}")

    // Detect lazy vals in both classes
    val detector = LazyValDetector()
    val result1 = detector.detect(class1)
    val result2 = detector.detect(class2)

    (result1, result2) match
      case (LazyValDetectionResult.NoLazyVals, LazyValDetectionResult.NoLazyVals) =>
        debug("Neither class has lazy vals")
        LazyValComparisonResult.BothNoLazyVals

      case (LazyValDetectionResult.NoLazyVals, _) =>
        val lazyVals2 = extractLazyVals(result2)
        debug(s"Only second class has lazy vals (${lazyVals2.size})")
        LazyValComparisonResult.OnlyOneHasLazyVals(false, lazyVals2)

      case (_, LazyValDetectionResult.NoLazyVals) =>
        val lazyVals1 = extractLazyVals(result1)
        debug(s"Only first class has lazy vals (${lazyVals1.size})")
        LazyValComparisonResult.OnlyOneHasLazyVals(true, lazyVals1)

      case _ =>
        val lazyVals1 = extractLazyVals(result1)
        val lazyVals2 = extractLazyVals(result2)
        val version1 = getOverallVersion(result1)
        val version2 = getOverallVersion(result2)

        if version1 == version2 then
          info(s"Both classes use same lazy val implementation: $version1")
          LazyValComparisonResult.SameImplementation(version1, lazyVals1, lazyVals2)
        else
          info(s"Different lazy val implementations: $version1 vs $version2")
          val differences = computeDifferences(lazyVals1, lazyVals2)
          LazyValComparisonResult.DifferentImplementations(
            version1,
            version2,
            lazyVals1,
            lazyVals2,
            differences
          )

  /** Extracts lazy vals from detection result. */
  private def extractLazyVals(result: LazyValDetectionResult): Seq[LazyValInfo] =
    result match
      case LazyValDetectionResult.NoLazyVals                 => Seq.empty
      case LazyValDetectionResult.LazyValsFound(lazyVals, _) => lazyVals
      case LazyValDetectionResult.MixedVersions(lazyVals)    => lazyVals

  /** Gets overall version from detection result. */
  private def getOverallVersion(result: LazyValDetectionResult): ScalaVersion =
    result match
      case LazyValDetectionResult.NoLazyVals                => ScalaVersion.Unknown("no lazy vals detected")
      case LazyValDetectionResult.LazyValsFound(_, version) => version
      case LazyValDetectionResult.MixedVersions(lazyVals)   =>
        // Return most common version
        lazyVals.groupBy(_.version).maxBy(_._2.size)._1

  /** Computes detailed differences between two lazy val sequences. */
  private def computeDifferences(
      lazyVals1: Seq[LazyValInfo],
      lazyVals2: Seq[LazyValInfo]
  ): Seq[LazyValDifference] =
    val map1 = lazyVals1.map(lv => (lv.name, lv.index) -> lv).toMap
    val map2 = lazyVals2.map(lv => (lv.name, lv.index) -> lv).toMap
    val allKeys = (map1.keySet ++ map2.keySet).toSeq.sortBy { case (name, idx) => (name, idx) }

    allKeys.flatMap { key =>
      val (name, index) = key
      (map1.get(key), map2.get(key)) match
        case (Some(lv1), Some(lv2)) =>
          compareLazyVals(lv1, lv2)

        case (Some(_), None) =>
          Seq(LazyValDifference.OnlyInOne(name, index, inFirst = true))

        case (None, Some(_)) =>
          Seq(LazyValDifference.OnlyInOne(name, index, inFirst = false))

        case _ =>
          Seq.empty
    }

  /** Compares two lazy vals with the same name and index. */
  private def compareLazyVals(
      lv1: LazyValInfo,
      lv2: LazyValInfo
  ): Seq[LazyValDifference] =
    val diffs = scala.collection.mutable.ArrayBuffer[LazyValDifference]()

    // Check version difference
    if lv1.version != lv2.version then
      diffs += LazyValDifference.VersionDiffers(
        lv1.name,
        lv1.index,
        lv1.version,
        lv2.version
      )

    // Check field structure differences
    val fieldDiffs = compareFieldStructure(lv1, lv2)
    if fieldDiffs.nonEmpty then
      diffs += LazyValDifference.FieldStructureDiffers(
        lv1.name,
        lv1.index,
        fieldDiffs.mkString(", ")
      )

    // Check method structure differences
    val methodDiffs = compareMethodStructure(lv1, lv2)
    if methodDiffs.nonEmpty then
      diffs += LazyValDifference.MethodStructureDiffers(
        lv1.name,
        lv1.index,
        methodDiffs.mkString(", ")
      )

    diffs.toSeq

  /** Compares field structure between two lazy vals. */
  private def compareFieldStructure(
      lv1: LazyValInfo,
      lv2: LazyValInfo
  ): Seq[String] =
    val diffs = scala.collection.mutable.ArrayBuffer[String]()

    // Check offset field
    (lv1.offsetField, lv2.offsetField) match
      case (Some(_), None) => diffs += "offset field only in first"
      case (None, Some(_)) => diffs += "offset field only in second"
      case _               => ()

    // Check bitmap field
    (lv1.bitmapField, lv2.bitmapField) match
      case (Some(_), None) => diffs += "bitmap field only in first"
      case (None, Some(_)) => diffs += "bitmap field only in second"
      case _               => ()

    // Check VarHandle field
    (lv1.varHandleField, lv2.varHandleField) match
      case (Some(_), None) => diffs += "VarHandle field only in first"
      case (None, Some(_)) => diffs += "VarHandle field only in second"
      case _               => ()

    // Check storage field descriptor
    if lv1.storageField.descriptor != lv2.storageField.descriptor then
      diffs += s"storage descriptor: ${lv1.storageField.descriptor} vs ${lv2.storageField.descriptor}"

    diffs.toSeq

  /** Compares method structure between two lazy vals. */
  private def compareMethodStructure(
      lv1: LazyValInfo,
      lv2: LazyValInfo
  ): Seq[String] =
    val diffs = scala.collection.mutable.ArrayBuffer[String]()

    // Check init method
    (lv1.initMethod, lv2.initMethod) match
      case (Some(_), None) => diffs += "lzyINIT method only in first"
      case (None, Some(_)) => diffs += "lzyINIT method only in second"
      case (Some(m1), Some(m2)) if m1.instructions.size != m2.instructions.size =>
        diffs += s"lzyINIT size: ${m1.instructions.size} vs ${m2.instructions.size} instructions"
      case _ => ()

    // Check accessor method
    (lv1.accessorMethod, lv2.accessorMethod) match
      case (Some(_), None) => diffs += "accessor method only in first"
      case (None, Some(_)) => diffs += "accessor method only in second"
      case (Some(m1), Some(m2)) if m1.instructions.size != m2.instructions.size =>
        diffs += s"accessor size: ${m1.instructions.size} vs ${m2.instructions.size} instructions"
      case _ => ()

    diffs.toSeq

object LazyValComparator:
  /** Creates a new comparator instance. */
  def apply(): LazyValComparator = new LazyValComparator()

  /** Convenience method to compare lazy vals in two classes. */
  def compare(class1: ClassInfo, class2: ClassInfo): LazyValComparisonResult =
    LazyValComparator().compare(class1, class2)
