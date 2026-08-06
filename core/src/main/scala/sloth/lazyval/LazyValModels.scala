package sloth.lazyval

import sloth.classfile.{FieldInfo, MethodInfo}

/** Location of the OFFSET field in companion object/class scenarios. */
enum OffsetFieldLocation:
  /** OFFSET field is in the same class as the lazy val implementation */
  case InSameClass

  /** OFFSET field is in the companion class (while lazy val impl is in companion object) */
  case InCompanionClass

  /** No OFFSET field present (e.g., 3.8+ VarHandle, or no lazy vals) */
  case NoOffsetField

/** Scala compiler version family for lazy val implementation. */
enum ScalaVersion:
  /** Scala 3.0.x - 3.1.x: Bitmap-based inline with LazyVals$.getOffset */
  case Scala30x_31x

  /** Scala 3.2.x: Bitmap-based inline with getDeclaredField + getOffsetStatic */
  case Scala32x

  /** Scala 3.3.x - 3.7.x: Object-based with lzyINIT and Unsafe */
  case Scala33x_37x

  /** Scala 3.8.0+: VarHandle-based implementation */
  case Scala38Plus

  /** Could not determine version (no lazy vals or unrecognized pattern) */
  case Unknown(reason: String)

  def isLegacy: Boolean = this match
    case Scala30x_31x | Scala32x | Scala33x_37x => true
    case _                                      => false

  def isBitmapBased: Boolean = this match
    case Scala30x_31x | Scala32x => true
    case _                       => false

  def needsTransformation: Boolean = this match
    case Scala30x_31x | Scala32x | Scala33x_37x => true
    case _                                      => false

/** Information about a single lazy val found in a class. */
final case class LazyValInfo(
    /** The name of the lazy val (without $lzy suffix) */
    name: String,

    /** The index/number of this lazy val (from $lzy<N> pattern) */
    index: Int,

    /** The offset field (OFFSET$_m_<N>) - present in 3.0-3.7.x */
    offsetField: Option[FieldInfo],

    /** Location of the offset field (same class vs companion class) */
    offsetFieldLocation: OffsetFieldLocation,

    /** The bitmap field (<N>bitmap$<M>) - present only in 3.0-3.2.x */
    bitmapField: Option[FieldInfo],

    /** The storage field (<name>$lzy<N>) - typed static in 3.0-3.2.x, volatile Object in 3.3+ */
    storageField: FieldInfo,

    /** The VarHandle field (<name>$lzy<N>$lzyHandle) - present only in 3.8+ */
    varHandleField: Option[FieldInfo],

    /** The initialization method (<name>$lzyINIT<N>) - present in 3.3+ */
    initMethod: Option[MethodInfo],

    /** The accessor method (<name>) */
    accessorMethod: Option[MethodInfo],

    /** Detected Scala version for this lazy val */
    version: ScalaVersion
)

/** Detection result for a class file. */
sealed trait LazyValDetectionResult

object LazyValDetectionResult:
  /** No lazy vals found in the class */
  case object NoLazyVals extends LazyValDetectionResult

  /** Lazy vals found and successfully classified */
  final case class LazyValsFound(
      lazyVals: Seq[LazyValInfo],
      overallVersion: ScalaVersion
  ) extends LazyValDetectionResult

  /** Mixed versions detected (should not happen in practice) */
  final case class MixedVersions(
      lazyVals: Seq[LazyValInfo]
  ) extends LazyValDetectionResult

/** Pattern matching result for bytecode sequences. */
sealed trait PatternMatch:
  def matched: Boolean

object PatternMatch:
  case object NoMatch extends PatternMatch:
    def matched: Boolean = false

  final case class Match(evidence: String) extends PatternMatch:
    def matched: Boolean = true

/** Detection evidence for a specific version. */
final case class VersionEvidence(
    version: ScalaVersion,
    confidence: Double,
    reasons: Seq[String]
)
