package sloth.lazyval

import sloth.classfile.{ClassInfo, MethodInfo}

/** Result of semantic lazy val comparison.
  *
  * Answers: "Are the lazy val implementations identical?"
  */
sealed trait SemanticLazyValComparisonResult:
  def areIdentical: Boolean

object SemanticLazyValComparisonResult:
  /** Both classes have identical lazy val implementations */
  case object Identical extends SemanticLazyValComparisonResult:
    def areIdentical: Boolean = true

  /** Lazy val implementations differ */
  final case class Different(reasons: Seq[String]) extends SemanticLazyValComparisonResult:
    def areIdentical: Boolean = false

  /** Both classes have no lazy vals (trivially identical) */
  case object BothNoLazyVals extends SemanticLazyValComparisonResult:
    def areIdentical: Boolean = true

  /** Only one class has lazy vals (different) */
  final case class OnlyOneHasLazyVals(firstHas: Boolean, count: Int) extends SemanticLazyValComparisonResult:
    def areIdentical: Boolean = false

/** Semantic comparator for lazy val implementations.
  *
  * Compares lazy vals based on their implementation patterns, ignoring all other bytecode differences.
  *
  * Version-aware comparison:
  *   - 3.0.x/3.1.x use same pattern (bitmap-based with getOffset)
  *   - 3.2.x uses similar bitmap pattern (with getDeclaredField)
  *   - 3.3-3.7.x use same pattern (object-based with Unsafe)
  *   - 3.8+ uses new pattern (object-based with VarHandle)
  */
final class SemanticLazyValComparator:
  import scribe.{debug, trace}

  /** Compares lazy val implementations semantically.
    *
    * @param class1
    *   First class
    * @param class2
    *   Second class
    * @return
    *   Whether lazy val implementations are identical
    */
  def compare(
      class1: ClassInfo,
      class2: ClassInfo,
      companion1: Option[ClassInfo] = None,
      companion2: Option[ClassInfo] = None
  ): SemanticLazyValComparisonResult =
    debug(s"Semantic comparison of lazy vals: ${class1.name} vs ${class2.name}")
    companion1.foreach(c => debug(s"  class1 companion: ${c.name}"))
    companion2.foreach(c => debug(s"  class2 companion: ${c.name}"))

    // Detect lazy vals in both classes
    val detector = LazyValDetector()
    val result1 = detector.detect(class1, companion1)
    val result2 = detector.detect(class2, companion2)

    (result1, result2) match
      case (LazyValDetectionResult.NoLazyVals, LazyValDetectionResult.NoLazyVals) =>
        debug("Both classes have no lazy vals")
        SemanticLazyValComparisonResult.BothNoLazyVals

      case (LazyValDetectionResult.NoLazyVals, _) =>
        val count = extractLazyVals(result2).size
        debug(s"Only second class has lazy vals ($count)")
        SemanticLazyValComparisonResult.OnlyOneHasLazyVals(firstHas = false, count)

      case (_, LazyValDetectionResult.NoLazyVals) =>
        val count = extractLazyVals(result1).size
        debug(s"Only first class has lazy vals ($count)")
        SemanticLazyValComparisonResult.OnlyOneHasLazyVals(firstHas = true, count)

      case _ =>
        val lazyVals1 = extractLazyVals(result1)
        val lazyVals2 = extractLazyVals(result2)
        compareImplementations(lazyVals1, lazyVals2, class1, class2, companion1, companion2)

  /** Extracts lazy vals from detection result. */
  private def extractLazyVals(result: LazyValDetectionResult): Seq[LazyValInfo] =
    result match
      case LazyValDetectionResult.NoLazyVals                 => Seq.empty
      case LazyValDetectionResult.LazyValsFound(lazyVals, _) => lazyVals
      case LazyValDetectionResult.MixedVersions(lazyVals)    => lazyVals

  /** Compares two sequences of lazy vals semantically. */
  private def compareImplementations(
      lazyVals1: Seq[LazyValInfo],
      lazyVals2: Seq[LazyValInfo],
      class1: ClassInfo,
      class2: ClassInfo,
      companion1: Option[ClassInfo],
      companion2: Option[ClassInfo]
  ): SemanticLazyValComparisonResult =
    val reasons = scala.collection.mutable.ArrayBuffer[String]()

    // Create maps keyed by lazy val name (primary identity)
    val map1 = lazyVals1.map(lv => lv.name -> lv).toMap
    val map2 = lazyVals2.map(lv => lv.name -> lv).toMap

    // Check for lazy vals only in one class
    val onlyIn1 = map1.keySet -- map2.keySet
    val onlyIn2 = map2.keySet -- map1.keySet

    if onlyIn1.nonEmpty then reasons += s"Lazy vals only in first: ${onlyIn1.mkString(", ")}"

    if onlyIn2.nonEmpty then reasons += s"Lazy vals only in second: ${onlyIn2.mkString(", ")}"

    // Compare lazy vals present in both
    val common = map1.keySet.intersect(map2.keySet)
    trace(s"Comparing ${common.size} common lazy vals")

    for name <- common.toSeq.sorted do
      val lv1 = map1(name)
      val lv2 = map2(name)

      // Determine which class contains this lazy val and where OFFSET field is
      val containingClass1 = determineContainingClass(lv1, class1, companion1)
      val containingClass2 = determineContainingClass(lv2, class2, companion2)

      // For OFFSET initialization, check companion class if OFFSET is located there
      val offsetClass1 =
        if lv1.offsetFieldLocation == OffsetFieldLocation.InCompanionClass then companion1.getOrElse(containingClass1)
        else containingClass1

      val offsetClass2 =
        if lv2.offsetFieldLocation == OffsetFieldLocation.InCompanionClass then companion2.getOrElse(containingClass2)
        else containingClass2

      // Extract canonical pattern for each
      val pattern1 = extractCanonicalPattern(lv1, containingClass1, offsetClass1)
      val pattern2 = extractCanonicalPattern(lv2, containingClass2, offsetClass2)

      if pattern1 != pattern2 then
        val diff = describePatternDifference(name, pattern1, pattern2)
        reasons += diff
        trace(s"Lazy val $name differs: $diff")
      else trace(s"Lazy val $name: identical pattern")

    if reasons.isEmpty then
      debug("All lazy val implementations are identical")
      SemanticLazyValComparisonResult.Identical
    else
      debug(s"Lazy val implementations differ: ${reasons.size} reasons")
      SemanticLazyValComparisonResult.Different(reasons.toSeq)

  /** Determines which class contains the lazy val implementation. */
  private def determineContainingClass(
      lv: LazyValInfo,
      mainClass: ClassInfo,
      companion: Option[ClassInfo]
  ): ClassInfo =
    // Check if accessor method is in main class or companion
    lv.accessorMethod match
      case Some(accessor) =>
        if mainClass.methods.exists(_.name == accessor.name) then mainClass
        else companion.getOrElse(mainClass)
      case None => mainClass

  /** Extracts a canonical pattern signature for a lazy val.
    *
    * This captures the essential implementation characteristics for the detected version by extracting synchronization
    * skeleton.
    *
    * @param lv
    *   The lazy val info
    * @param containingClass
    *   The class containing the lazy val implementation
    * @param offsetClass
    *   The class containing the OFFSET field initialization (may be companion class)
    */
  private def extractCanonicalPattern(
      lv: LazyValInfo,
      containingClass: ClassInfo,
      offsetClass: ClassInfo
  ): LazyValCanonicalPattern =
    lv.version match
      case ScalaVersion.Scala30x_31x | ScalaVersion.Scala32x =>
        // Bitmap-based inline pattern
        val accessorSkeleton = lv.accessorMethod.map(extractSynchronizationSkeleton)
        // Extract OFFSET initialization pattern from <clinit> of the class that has OFFSET
        val offsetInitPattern = lv.offsetField.flatMap(_ => extractOffsetInitPattern(offsetClass))
        LazyValCanonicalPattern.BitmapBased(
          hasOffsetField = lv.offsetField.isDefined,
          hasBitmapField = lv.bitmapField.isDefined,
          storageDescriptor = lv.storageField.descriptor,
          hasInitMethod = lv.initMethod.isDefined, // Should be false
          accessorSkeleton = accessorSkeleton,
          offsetInitPattern = offsetInitPattern
        )

      case ScalaVersion.Scala33x_37x =>
        // Object-based with Unsafe
        val initSkeleton = lv.initMethod.map(extractSynchronizationSkeleton)
        val accessorSkeleton = lv.accessorMethod.map(extractSynchronizationSkeleton)
        LazyValCanonicalPattern.ObjectBasedUnsafe(
          hasOffsetField = lv.offsetField.isDefined,
          storageDescriptor = lv.storageField.descriptor,
          hasInitMethod = lv.initMethod.isDefined,
          initSkeleton = initSkeleton,
          accessorSkeleton = accessorSkeleton
        )

      case ScalaVersion.Scala38Plus =>
        // Object-based with VarHandle
        val initSkeleton = lv.initMethod.map(extractSynchronizationSkeleton)
        val accessorSkeleton = lv.accessorMethod.map(extractSynchronizationSkeleton)
        LazyValCanonicalPattern.ObjectBasedVarHandle(
          hasVarHandleField = lv.varHandleField.isDefined,
          storageDescriptor = lv.storageField.descriptor,
          hasInitMethod = lv.initMethod.isDefined,
          initSkeleton = initSkeleton,
          accessorSkeleton = accessorSkeleton
        )

      case ScalaVersion.Unknown(_) =>
        LazyValCanonicalPattern.Unknown

  /** Extracts OFFSET field initialization pattern from <clinit> static initializer.
    *
    * This captures how the OFFSET field is computed, which differs between versions:
    *   - 3.0/3.1: LazyVals$.getOffset(Class, String)
    *   - 3.2: Class.getDeclaredField(String) + LazyVals$.getOffsetStatic(Field)
    *   - 3.3+: Uses OFFSET in object-based pattern differently
    */
  private def extractOffsetInitPattern(classInfo: ClassInfo): Option[String] =
    // Find <clinit> method
    val clinit = classInfo.methods.find(m => m.name == "<clinit>")

    clinit.flatMap { method =>
      val instructions = method.instructions

      // Look for OFFSET initialization patterns
      val hasGetOffset = instructions.exists(insn =>
        insn.details.contains("LazyVals$.getOffset") || insn.details.contains("LazyVals.getOffset")
      )
      val hasGetDeclaredField = instructions.exists(insn => insn.details.contains("getDeclaredField"))
      val hasGetOffsetStatic = instructions.exists(insn => insn.details.contains("getOffsetStatic"))

      if hasGetOffset && !hasGetDeclaredField then Some("LazyVals.getOffset") // 3.0/3.1 pattern
      else if hasGetDeclaredField && hasGetOffsetStatic then Some("getDeclaredField+getOffsetStatic") // 3.2 pattern
      else None
    }

  /** Extracts synchronization skeleton from a method.
    *
    * This extracts only the lazy val synchronization operations, ignoring the actual computation (body) of the lazy
    * val.
    *
    * The skeleton preserves order of operations, which is critical for thread safety semantics.
    *
    * @param method
    *   The method to extract from (accessor or init method)
    * @return
    *   Sequence of synchronization instruction patterns
    */
  private def extractSynchronizationSkeleton(method: MethodInfo): Seq[String] =
    import sloth.classfile.InstructionInfo

    val skeleton = scala.collection.mutable.ArrayBuffer[String]()
    var lastOpcode: Option[String] = None

    for insn <- method.instructions do
      val normalized = normalizeSyncInstruction(insn, lastOpcode)
      normalized.foreach(skeleton += _)
      lastOpcode = Some(insn.opcodeString)

    skeleton.toSeq

  /** Normalizes a bytecode instruction to synchronization pattern form.
    *
    * Returns Some(pattern) if this instruction is part of synchronization logic, None if it should be ignored (part of
    * lazy val body).
    */
  private def normalizeSyncInstruction(
      insn: sloth.classfile.InstructionInfo,
      lastOpcode: Option[String]
  ): Option[String] =
    val details = insn.details.trim
    val opcode = insn.opcodeString

    // Synchronization field patterns
    if details.contains("bitmap$") then Some(s"GETFIELD bitmap")
    else if details.contains("OFFSET$") then Some(s"GETFIELD OFFSET")
    else if details.contains("$lzy") && details.contains("GETFIELD") && !details.contains("lzyHandle") then
      Some(s"GETFIELD storage")
    else if details.contains("$lzy") && details.contains("PUTFIELD") && !details.contains("lzyHandle") then
      Some(s"PUTFIELD storage")
    else if details.contains("lzyHandle") && details.contains("GETFIELD") then Some(s"GETFIELD varhandle")
    // Synchronization operations
    else if opcode == "MONITORENTER" then Some("MONITORENTER")
    else if opcode == "MONITOREXIT" then Some("MONITOREXIT")
    // Unsafe CAS operations
    else if details.contains("objCAS") || details.contains("compareAndSet") then Some("CAS")
    // VarHandle operations
    else if details.contains("VarHandle") && details.contains("invoke") then Some("VARHANDLE_OP")
    // Bitwise operations on bitmaps/flags
    else if (opcode == "IAND" || opcode == "IOR") && lastOpcode.exists(_ == "GETFIELD") then Some(s"BITOP $opcode")
    // Conditional jumps for flag checks (thread safety)
    else if opcode.startsWith("IF") && lastOpcode.exists(op => op == "IAND" || op == "IOR") then
      Some(s"CONDITIONAL $opcode")
    // Stack operations immediately following sync field access (needed for CAS setup)
    else if (opcode == "ALOAD" || opcode == "ASTORE") && lastOpcode.exists(_.contains("FIELD")) then
      Some(s"STACK_SYNC $opcode")
    else if opcode == "DUP" && lastOpcode.exists(_.contains("FIELD")) then Some(s"STACK_SYNC DUP")
    // Ignore everything else (lazy val body computation)
    else None

  /** Describes the difference between two patterns. */
  private def describePatternDifference(
      name: String,
      p1: LazyValCanonicalPattern,
      p2: LazyValCanonicalPattern
  ): String =
    (p1, p2) match
      case (
            LazyValCanonicalPattern.BitmapBased(_, _, desc1, _, acc1, offset1),
            LazyValCanonicalPattern.BitmapBased(_, _, desc2, _, acc2, offset2)
          ) =>
        if desc1 != desc2 then s"Lazy val '$name': storage type differs ($desc1 vs $desc2)"
        else if offset1 != offset2 then s"Lazy val '$name': OFFSET initialization differs ($offset1 vs $offset2)"
        else if acc1 != acc2 then s"Lazy val '$name': accessor synchronization skeleton differs"
        else s"Lazy val '$name': bitmap-based patterns differ in structure"

      case (
            LazyValCanonicalPattern.ObjectBasedUnsafe(_, desc1, _, init1, acc1),
            LazyValCanonicalPattern.ObjectBasedUnsafe(_, desc2, _, init2, acc2)
          ) =>
        if desc1 != desc2 then s"Lazy val '$name': storage type differs ($desc1 vs $desc2)"
        else if init1 != init2 then s"Lazy val '$name': init method synchronization skeleton differs"
        else if acc1 != acc2 then s"Lazy val '$name': accessor synchronization skeleton differs"
        else s"Lazy val '$name': Unsafe-based patterns differ in structure"

      case (
            LazyValCanonicalPattern.ObjectBasedVarHandle(_, desc1, _, init1, acc1),
            LazyValCanonicalPattern.ObjectBasedVarHandle(_, desc2, _, init2, acc2)
          ) =>
        if desc1 != desc2 then s"Lazy val '$name': storage type differs ($desc1 vs $desc2)"
        else if init1 != init2 then s"Lazy val '$name': init method synchronization skeleton differs"
        else if acc1 != acc2 then s"Lazy val '$name': accessor synchronization skeleton differs"
        else s"Lazy val '$name': VarHandle-based patterns differ in structure"

      case _ =>
        s"Lazy val '$name': different implementation versions ($p1 vs $p2)"

/** Canonical pattern signature for a lazy val implementation. */
private sealed trait LazyValCanonicalPattern

private object LazyValCanonicalPattern:
  /** Bitmap-based implementation (3.0-3.2) */
  final case class BitmapBased(
      hasOffsetField: Boolean,
      hasBitmapField: Boolean,
      storageDescriptor: String,
      hasInitMethod: Boolean,
      accessorSkeleton: Option[Seq[String]],
      offsetInitPattern: Option[String]
  ) extends LazyValCanonicalPattern

  /** Object-based with Unsafe (3.3-3.7) */
  final case class ObjectBasedUnsafe(
      hasOffsetField: Boolean,
      storageDescriptor: String,
      hasInitMethod: Boolean,
      initSkeleton: Option[Seq[String]],
      accessorSkeleton: Option[Seq[String]]
  ) extends LazyValCanonicalPattern

  /** Object-based with VarHandle (3.8+) */
  final case class ObjectBasedVarHandle(
      hasVarHandleField: Boolean,
      storageDescriptor: String,
      hasInitMethod: Boolean,
      initSkeleton: Option[Seq[String]],
      accessorSkeleton: Option[Seq[String]]
  ) extends LazyValCanonicalPattern

  /** Unknown pattern */
  case object Unknown extends LazyValCanonicalPattern

object SemanticLazyValComparator:
  /** Creates a new semantic comparator instance. */
  def apply(): SemanticLazyValComparator = new SemanticLazyValComparator()

  /** Convenience method to compare lazy vals semantically. */
  def compare(
      class1: ClassInfo,
      class2: ClassInfo,
      companion1: Option[ClassInfo] = None,
      companion2: Option[ClassInfo] = None
  ): SemanticLazyValComparisonResult =
    SemanticLazyValComparator().compare(class1, class2, companion1, companion2)
