package sloth.classfile

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.util._
import scala.jdk.CollectionConverters._
import scala.util.{Try, Success, Failure}
import java.io.{StringWriter, PrintWriter}

/** Parser for Java classfiles using ASM library.
  *
  * Provides composable, testable parsing of classfiles into immutable domain models. All methods return Try or Either
  * to avoid throwing exceptions.
  */
final class ClassfileParser:
  import scribe.{info, debug, warn, error}

  /** Parses a classfile from byte array.
    *
    * @param bytes
    *   The classfile bytes
    * @return
    *   Right with ClassInfo on success, Left with error on failure
    */
  def parseClassfile(bytes: Array[Byte]): Either[ClassfileError, ClassInfo] =
    info(s"Starting classfile parse (${bytes.length} bytes)")

    for
      classNode <- readClassNode(bytes)
      fullDump <- generateFullDump(bytes)
      classInfo <- buildClassInfo(classNode, fullDump)
    yield
      info(
        s"Successfully parsed class: ${classInfo.name} with ${classInfo.methods.size} methods, ${classInfo.fields.size} fields"
      )
      classInfo

  /** Parses a classfile from byte array, returning Try.
    *
    * @param bytes
    *   The classfile bytes
    * @return
    *   Success with ClassInfo or Failure with exception
    */
  def parseClassfileTry(bytes: Array[Byte]): Try[ClassInfo] =
    Try:
      parseClassfile(bytes) match
        case Right(info) => info
        case Left(error) =>
          throw new Exception(s"${error.message}", error.cause.orNull)

  /** Reads a ClassNode from bytes using ASM.
    *
    * @param bytes
    *   The classfile bytes
    * @return
    *   Right with ClassNode on success, Left with error on failure
    */
  private def readClassNode(bytes: Array[Byte]): Either[ClassfileError, ClassNode] =
    Try:
      val classNode = new ClassNode()
      val classReader = new ClassReader(bytes)
      classReader.accept(classNode, ClassReader.EXPAND_FRAMES)
      debug(s"Read ClassNode for: ${classNode.name}")
      classNode
    .toEither.left.map: ex =>
      error(s"Failed to read classfile", ex)
      ClassfileError.ReadError(s"Failed to read classfile: ${ex.getMessage}", Some(ex))

  /** Generates a full textual dump of the classfile.
    *
    * @param bytes
    *   The classfile bytes
    * @return
    *   Right with dump string on success, Left with error on failure
    */
  private def generateFullDump(bytes: Array[Byte]): Either[ClassfileError, String] =
    Try:
      val stringWriter = new StringWriter()
      val printWriter = new PrintWriter(stringWriter)
      val textifier = new Textifier()
      val traceClassVisitor = new TraceClassVisitor(null, textifier, printWriter)
      val classReader = new ClassReader(bytes)
      classReader.accept(traceClassVisitor, ClassReader.EXPAND_FRAMES)
      val dump = stringWriter.toString
      debug(s"Generated full dump (${dump.length} chars)")
      dump
    .toEither.left.map: ex =>
      error(s"Failed to generate bytecode dump", ex)
      ClassfileError.DumpError(s"Failed to generate dump: ${ex.getMessage}", Some(ex))

  /** Builds ClassInfo from a ClassNode and full dump.
    *
    * @param classNode
    *   The ASM ClassNode
    * @param fullDump
    *   The full textual dump of the class
    * @return
    *   Right with ClassInfo on success, Left with error on failure
    */
  private def buildClassInfo(classNode: ClassNode, fullDump: String): Either[ClassfileError, ClassInfo] =
    Try:
      val fields = parseFields(classNode)
      val methods = parseMethods(classNode, fullDump)

      ClassInfo(
        name = classNode.name,
        superName = classNode.superName,
        interfaces = Option(classNode.interfaces)
          .map(_.asScala.toSeq)
          .getOrElse(Seq.empty),
        access = classNode.access,
        fields = fields,
        methods = methods
      )
    .toEither.left.map: ex =>
      error(s"Failed to build ClassInfo", ex)
      ClassfileError.InvalidClassfile(
        s"Failed to build ClassInfo: ${ex.getMessage}",
        Some(ex)
      )

  /** Parses field information from a ClassNode.
    *
    * @param classNode
    *   The ASM ClassNode
    * @return
    *   Sequence of FieldInfo
    */
  private def parseFields(classNode: ClassNode): Seq[FieldInfo] =
    val fields = Option(classNode.fields)
      .map:
        _.asScala
          .map: field =>
            FieldInfo(
              name = field.name,
              descriptor = field.desc,
              access = field.access,
              signature = Option(field.signature)
            )
          .toSeq
      .getOrElse(Seq.empty)

    debug(s"Parsed ${fields.size} fields")
    fields

  /** Parses method information from a ClassNode.
    *
    * @param classNode
    *   The ASM ClassNode
    * @param fullDump
    *   The full textual dump for extracting method bytecode
    * @return
    *   Sequence of MethodInfo
    */
  private def parseMethods(classNode: ClassNode, fullDump: String): Seq[MethodInfo] =
    val methods = Option(classNode.methods)
      .map:
        _.asScala
          .map: method =>
            val instructions = parseInstructions(method)
            val localVars = parseLocalVariables(method)
            val exceptions = parseExceptions(method)
            val bytecodeText = extractMethodBytecode(fullDump, method.name, method.desc)

            MethodInfo(
              name = method.name,
              descriptor = method.desc,
              access = method.access,
              signature = Option(method.signature),
              instructions = instructions,
              localVariables = localVars,
              exceptions = exceptions,
              bytecodeText = bytecodeText
            )
          .toSeq
      .getOrElse(Seq.empty)

    debug(s"Parsed ${methods.size} methods")
    methods

  /** Parses instruction information from a method.
    *
    * @param method
    *   The ASM MethodNode
    * @return
    *   Sequence of InstructionInfo
    */
  private def parseInstructions(method: MethodNode): Seq[InstructionInfo] =
    Option(method.instructions)
      .map: insnList =>
        val insns = insnList.iterator().asScala.toSeq
        insns
          .map: insn =>
            val opcodeStr =
              if insn.getOpcode >= 0 then
                Try:
                  val printer = new Textifier()
                  val methodVisitor = new TraceMethodVisitor(printer)
                  insn.accept(methodVisitor)
                  printer.getText.asScala.headOption
                    .map(_.toString.trim)
                    .getOrElse("???")
                .getOrElse("???")
              else "LABEL/FRAME/LINE"

            InstructionInfo(
              opcode = insn.getOpcode,
              opcodeString =
                if insn.getOpcode >= 0 then OpcodeUtils.opcodeToString(insn.getOpcode)
                else "META",
              details = opcodeStr
            )
          .toSeq
      .getOrElse(Seq.empty)

  /** Parses local variable information from a method.
    *
    * @param method
    *   The ASM MethodNode
    * @return
    *   Sequence of local variable descriptions
    */
  private def parseLocalVariables(method: MethodNode): Seq[String] =
    Option(method.localVariables)
      .map: lvs =>
        lvs.asScala
          .map: lv =>
            s"${lv.name}: ${lv.desc}"
          .toSeq
      .getOrElse(Seq.empty)

  /** Parses exception information from a method.
    *
    * @param method
    *   The ASM MethodNode
    * @return
    *   Sequence of exception class names
    */
  private def parseExceptions(method: MethodNode): Seq[String] =
    Option(method.exceptions)
      .map:
        _.asScala.toSeq
      .getOrElse(Seq.empty)

  /** Extracts method bytecode text from the full dump.
    *
    * @param fullDump
    *   The full textual dump of the class
    * @param methodName
    *   The method name to extract
    * @param methodDesc
    *   The method descriptor
    * @return
    *   The bytecode text for the specified method
    */
  private def extractMethodBytecode(fullDump: String, methodName: String, methodDesc: String): String =
    val lines = fullDump.split("\n")
    val methodSignature = s"$methodName$methodDesc"

    val methodStart = lines.indexWhere: line =>
      line.trim.endsWith(methodSignature) || line.contains(s" $methodSignature")

    if methodStart >= 0 then
      val methodEnd = lines.indexWhere(
        line => {
          val trimmed = line.trim
          val startsNewSection = line.startsWith("  // access flags") &&
            !line.startsWith("    ") &&
            methodStart < lines.indexOf(line) - 1
          startsNewSection || (trimmed.startsWith("// access flags 0x") && !line
            .startsWith("    "))
        },
        methodStart + 1
      )

      val endIdx = if methodEnd > methodStart then methodEnd else lines.length
      lines.slice(methodStart, endIdx).mkString("\n")
    else
      warn(s"Method $methodName$methodDesc not found in dump")
      s"Method $methodName$methodDesc not found"

object ClassfileParser:

  /** Creates a new ClassfileParser instance. */
  def apply(): ClassfileParser = new ClassfileParser()

  /** Convenience method to parse a classfile.
    *
    * @param bytes
    *   The classfile bytes
    * @return
    *   Right with ClassInfo on success, Left with error on failure
    */
  def parse(bytes: Array[Byte]): Either[ClassfileError, ClassInfo] =
    ClassfileParser().parseClassfile(bytes)

  /** Convenience method to parse a classfile using Try.
    *
    * @param bytes
    *   The classfile bytes
    * @return
    *   Success with ClassInfo or Failure with exception
    */
  def parseTry(bytes: Array[Byte]): Try[ClassInfo] =
    ClassfileParser().parseClassfileTry(bytes)
