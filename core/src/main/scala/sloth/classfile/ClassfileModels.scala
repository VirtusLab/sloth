package sloth.classfile

/** Represents a field in a class file.
  *
  * @param name
  *   The field name
  * @param descriptor
  *   The field type descriptor (e.g., "Ljava/lang/String;")
  * @param access
  *   The access flags as a bitmask
  * @param signature
  *   Optional generic signature
  */
final case class FieldInfo(
    name: String,
    descriptor: String,
    access: Int,
    signature: Option[String]
)

/** Represents a single bytecode instruction.
  *
  * @param opcode
  *   The opcode value (negative for meta-instructions like labels)
  * @param opcodeString
  *   Human-readable opcode name
  * @param details
  *   Detailed instruction representation from ASM Textifier
  */
final case class InstructionInfo(
    opcode: Int,
    opcodeString: String,
    details: String
)

/** Represents a method in a class file.
  *
  * @param name
  *   The method name
  * @param descriptor
  *   The method descriptor (e.g., "(Ljava/lang/String;)V")
  * @param access
  *   The access flags as a bitmask
  * @param signature
  *   Optional generic signature
  * @param instructions
  *   The sequence of bytecode instructions
  * @param localVariables
  *   Local variable information
  * @param exceptions
  *   Declared exceptions
  * @param bytecodeText
  *   Full textual representation of the method bytecode
  */
final case class MethodInfo(
    name: String,
    descriptor: String,
    access: Int,
    signature: Option[String],
    instructions: Seq[InstructionInfo],
    localVariables: Seq[String],
    exceptions: Seq[String],
    bytecodeText: String
)

/** Represents a complete class file with all its members.
  *
  * @param name
  *   The internal class name (e.g., "com/example/MyClass")
  * @param superName
  *   The internal name of the superclass
  * @param interfaces
  *   The internal names of implemented interfaces
  * @param access
  *   The access flags as a bitmask
  * @param fields
  *   All fields declared in this class
  * @param methods
  *   All methods declared in this class
  */
final case class ClassInfo(
    name: String,
    superName: String,
    interfaces: Seq[String],
    access: Int,
    fields: Seq[FieldInfo],
    methods: Seq[MethodInfo]
)

/** Errors that can occur during classfile parsing. */
sealed trait ClassfileError {
  def message: String
  def cause: Option[Throwable]
}

object ClassfileError {

  /** Error during ASM class reading. */
  final case class ReadError(message: String, cause: Option[Throwable] = None) extends ClassfileError

  /** Error during bytecode dump generation. */
  final case class DumpError(message: String, cause: Option[Throwable] = None) extends ClassfileError

  /** Invalid or corrupted classfile. */
  final case class InvalidClassfile(
      message: String,
      cause: Option[Throwable] = None
  ) extends ClassfileError
}
