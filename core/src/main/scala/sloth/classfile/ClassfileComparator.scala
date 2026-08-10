package sloth.classfile

/** Result of comparing two class files. */
sealed trait ComparisonResult

object ComparisonResult {

  /** The classfiles are fully identical (byte-for-byte). */
  case object FullyIdentical extends ComparisonResult

  /** The classfiles differ in some way. */
  final case class Different(differences: Seq[Difference]) extends ComparisonResult

  /** A specific difference between two classfiles. */
  sealed trait Difference {
    def description: String
  }

  object Difference {

    /** A field is present in only one class. */
    final case class FieldOnlyIn(
        fieldName: String,
        presentIn: WhichClass,
        fieldInfo: FieldInfo
    ) extends Difference {
      def description: String = s"Field $fieldName only in $presentIn"
    }

    /** A field differs between the two classes. */
    final case class FieldDiffers(
        fieldName: String,
        field1: FieldInfo,
        field2: FieldInfo
    ) extends Difference {
      def description: String = s"Field $fieldName differs"
    }

    /** A method is present in only one class. */
    final case class MethodOnlyIn(
        methodSignature: String,
        presentIn: WhichClass,
        methodInfo: MethodInfo
    ) extends Difference {
      def description: String = s"Method $methodSignature only in $presentIn"
    }

    /** A method differs between the two classes. */
    final case class MethodDiffers(
        methodSignature: String,
        method1: MethodInfo,
        method2: MethodInfo,
        details: MethodDifferenceDetails
    ) extends Difference {
      def description: String = {
        val detailsStr = Seq(
          if (details.accessDiffers) Some("access flags") else None,
          if (details.instructionsDiffer) Some("instructions") else None,
          if (details.bytecodeDiffers && !details.instructionsDiffer)
            Some("bytecode (metadata/frames)")
          else None
        ).flatten.mkString(", ")

        s"Method $methodSignature differs: $detailsStr"
      }
    }

    /** Details about how a method differs. */
    final case class MethodDifferenceDetails(
        accessDiffers: Boolean,
        instructionsDiffer: Boolean,
        bytecodeDiffers: Boolean
    )

    /** Indicates which of the two classes being compared. */
    sealed trait WhichClass
    object WhichClass {
      case object First extends WhichClass {
        override def toString: String = "first class"
      }
      case object Second extends WhichClass {
        override def toString: String = "second class"
      }
    }
  }
}

/** Comparator for classfiles.
  *
  * Provides composable, immutable comparison of ClassInfo instances.
  */
object ClassfileComparator {
  import ComparisonResult._
  import Difference._

  /** Compares two ClassInfo instances.
    *
    * @param class1
    *   First class to compare
    * @param class2
    *   Second class to compare
    * @param bytes1
    *   Optional raw bytes of first class (for full dump comparison)
    * @param bytes2
    *   Optional raw bytes of second class (for full dump comparison)
    * @return
    *   ComparisonResult indicating if classes are identical or how they differ
    */
  def compare(
      class1: ClassInfo,
      class2: ClassInfo,
      bytes1: Option[Array[Byte]] = None,
      bytes2: Option[Array[Byte]] = None
  ): ComparisonResult = {
    // If we have raw bytes, check if they're fully identical first
    (bytes1, bytes2) match {
      case (Some(b1), Some(b2)) if java.util.Arrays.equals(b1, b2) =>
        return FullyIdentical
      case _ => ()
    }

    val differences = scala.collection.mutable.ArrayBuffer[Difference]()

    // Compare fields
    differences ++= compareFields(class1.fields, class2.fields)

    // Compare methods
    differences ++= compareMethods(class1.methods, class2.methods)

    if (differences.isEmpty) FullyIdentical
    else Different(differences.toSeq)
  }

  /** Compares field sequences.
    *
    * @param fields1
    *   Fields from first class
    * @param fields2
    *   Fields from second class
    * @return
    *   Sequence of field differences
    */
  private def compareFields(
      fields1: Seq[FieldInfo],
      fields2: Seq[FieldInfo]
  ): Seq[Difference] = {
    val map1 = fields1.map(f => f.name -> f).toMap
    val map2 = fields2.map(f => f.name -> f).toMap
    val allNames = (map1.keySet ++ map2.keySet).toSeq.sorted

    allNames.flatMap { name =>
      (map1.get(name), map2.get(name)) match {
        case (Some(f1), Some(f2)) if f1 == f2 => None
        case (Some(f1), Some(f2)) =>
          Some(FieldDiffers(name, f1, f2))
        case (Some(f1), None) =>
          Some(FieldOnlyIn(name, WhichClass.First, f1))
        case (None, Some(f2)) =>
          Some(FieldOnlyIn(name, WhichClass.Second, f2))
        case _ => None
      }
    }
  }

  /** Compares method sequences.
    *
    * @param methods1
    *   Methods from first class
    * @param methods2
    *   Methods from second class
    * @return
    *   Sequence of method differences
    */
  private def compareMethods(
      methods1: Seq[MethodInfo],
      methods2: Seq[MethodInfo]
  ): Seq[Difference] = {
    val map1 = methods1.map(m => (m.name + m.descriptor) -> m).toMap
    val map2 = methods2.map(m => (m.name + m.descriptor) -> m).toMap
    val allSignatures = (map1.keySet ++ map2.keySet).toSeq.sorted

    allSignatures.flatMap { signature =>
      (map1.get(signature), map2.get(signature)) match {
        case (Some(m1), Some(m2)) =>
          val accessDiffers = m1.access != m2.access
          val instructionsDiffer = m1.instructions != m2.instructions
          val bytecodeDiffers = m1.bytecodeText != m2.bytecodeText

          if (accessDiffers || instructionsDiffer || bytecodeDiffers) {
            Some(
              MethodDiffers(
                signature,
                m1,
                m2,
                MethodDifferenceDetails(
                  accessDiffers,
                  instructionsDiffer,
                  bytecodeDiffers
                )
              )
            )
          } else None

        case (Some(m1), None) =>
          Some(MethodOnlyIn(signature, WhichClass.First, m1))
        case (None, Some(m2)) =>
          Some(MethodOnlyIn(signature, WhichClass.Second, m2))
        case _ => None
      }
    }
  }

  /** Checks if two classes are identical.
    *
    * @param class1
    *   First class
    * @param class2
    *   Second class
    * @param bytes1
    *   Optional raw bytes of first class
    * @param bytes2
    *   Optional raw bytes of second class
    * @return
    *   True if classes are identical, false otherwise
    */
  def areIdentical(
      class1: ClassInfo,
      class2: ClassInfo,
      bytes1: Option[Array[Byte]] = None,
      bytes2: Option[Array[Byte]] = None
  ): Boolean = compare(class1, class2, bytes1, bytes2) == FullyIdentical

  /** Gets all differences between two classes.
    *
    * @param class1
    *   First class
    * @param class2
    *   Second class
    * @param bytes1
    *   Optional raw bytes of first class
    * @param bytes2
    *   Optional raw bytes of second class
    * @return
    *   Sequence of differences, empty if classes are identical
    */
  def differences(
      class1: ClassInfo,
      class2: ClassInfo,
      bytes1: Option[Array[Byte]] = None,
      bytes2: Option[Array[Byte]] = None
  ): Seq[Difference] = compare(class1, class2, bytes1, bytes2) match {
    case FullyIdentical   => Seq.empty
    case Different(diffs) => diffs
  }
}
