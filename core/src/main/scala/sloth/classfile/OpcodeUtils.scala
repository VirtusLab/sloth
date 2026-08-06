package sloth.classfile

import org.objectweb.asm.Opcodes

/** Utilities for working with JVM opcodes and access flags. */
object OpcodeUtils:

  /** Converts an opcode integer to its human-readable string representation.
    *
    * @param opcode
    *   The opcode value
    * @return
    *   The opcode name (e.g., "ALOAD", "INVOKEVIRTUAL")
    */
  def opcodeToString(opcode: Int): String = opcode match
    case Opcodes.ALOAD           => "ALOAD"
    case Opcodes.ASTORE          => "ASTORE"
    case Opcodes.GETFIELD        => "GETFIELD"
    case Opcodes.PUTFIELD        => "PUTFIELD"
    case Opcodes.GETSTATIC       => "GETSTATIC"
    case Opcodes.PUTSTATIC       => "PUTSTATIC"
    case Opcodes.INVOKEVIRTUAL   => "INVOKEVIRTUAL"
    case Opcodes.INVOKESPECIAL   => "INVOKESPECIAL"
    case Opcodes.INVOKESTATIC    => "INVOKESTATIC"
    case Opcodes.INVOKEINTERFACE => "INVOKEINTERFACE"
    case Opcodes.INVOKEDYNAMIC   => "INVOKEDYNAMIC"
    case Opcodes.RETURN          => "RETURN"
    case Opcodes.ARETURN         => "ARETURN"
    case Opcodes.IRETURN         => "IRETURN"
    case Opcodes.LRETURN         => "LRETURN"
    case Opcodes.FRETURN         => "FRETURN"
    case Opcodes.DRETURN         => "DRETURN"
    case Opcodes.MONITORENTER    => "MONITORENTER"
    case Opcodes.MONITOREXIT     => "MONITOREXIT"
    case Opcodes.NEW             => "NEW"
    case Opcodes.DUP             => "DUP"
    case Opcodes.DUP_X1          => "DUP_X1"
    case Opcodes.DUP_X2          => "DUP_X2"
    case Opcodes.DUP2            => "DUP2"
    case Opcodes.DUP2_X1         => "DUP2_X1"
    case Opcodes.DUP2_X2         => "DUP2_X2"
    case Opcodes.POP             => "POP"
    case Opcodes.POP2            => "POP2"
    case Opcodes.SWAP            => "SWAP"
    case Opcodes.ICONST_0        => "ICONST_0"
    case Opcodes.ICONST_1        => "ICONST_1"
    case Opcodes.ICONST_2        => "ICONST_2"
    case Opcodes.ICONST_3        => "ICONST_3"
    case Opcodes.ICONST_4        => "ICONST_4"
    case Opcodes.ICONST_5        => "ICONST_5"
    case Opcodes.ICONST_M1       => "ICONST_M1"
    case Opcodes.LCONST_0        => "LCONST_0"
    case Opcodes.LCONST_1        => "LCONST_1"
    case Opcodes.FCONST_0        => "FCONST_0"
    case Opcodes.FCONST_1        => "FCONST_1"
    case Opcodes.FCONST_2        => "FCONST_2"
    case Opcodes.DCONST_0        => "DCONST_0"
    case Opcodes.DCONST_1        => "DCONST_1"
    case Opcodes.ACONST_NULL     => "ACONST_NULL"
    case Opcodes.BIPUSH          => "BIPUSH"
    case Opcodes.SIPUSH          => "SIPUSH"
    case Opcodes.LDC             => "LDC"
    case Opcodes.IF_ICMPNE       => "IF_ICMPNE"
    case Opcodes.IF_ICMPEQ       => "IF_ICMPEQ"
    case Opcodes.IF_ICMPLT       => "IF_ICMPLT"
    case Opcodes.IF_ICMPGE       => "IF_ICMPGE"
    case Opcodes.IF_ICMPGT       => "IF_ICMPGT"
    case Opcodes.IF_ICMPLE       => "IF_ICMPLE"
    case Opcodes.IF_ACMPEQ       => "IF_ACMPEQ"
    case Opcodes.IF_ACMPNE       => "IF_ACMPNE"
    case Opcodes.IFEQ            => "IFEQ"
    case Opcodes.IFNE            => "IFNE"
    case Opcodes.IFLT            => "IFLT"
    case Opcodes.IFGE            => "IFGE"
    case Opcodes.IFGT            => "IFGT"
    case Opcodes.IFLE            => "IFLE"
    case Opcodes.IFNULL          => "IFNULL"
    case Opcodes.IFNONNULL       => "IFNONNULL"
    case Opcodes.GOTO            => "GOTO"
    case Opcodes.JSR             => "JSR"
    case Opcodes.RET             => "RET"
    case Opcodes.TABLESWITCH     => "TABLESWITCH"
    case Opcodes.LOOKUPSWITCH    => "LOOKUPSWITCH"
    case Opcodes.ILOAD           => "ILOAD"
    case Opcodes.LLOAD           => "LLOAD"
    case Opcodes.FLOAD           => "FLOAD"
    case Opcodes.DLOAD           => "DLOAD"
    case Opcodes.ISTORE          => "ISTORE"
    case Opcodes.LSTORE          => "LSTORE"
    case Opcodes.FSTORE          => "FSTORE"
    case Opcodes.DSTORE          => "DSTORE"
    case Opcodes.IALOAD          => "IALOAD"
    case Opcodes.LALOAD          => "LALOAD"
    case Opcodes.FALOAD          => "FALOAD"
    case Opcodes.DALOAD          => "DALOAD"
    case Opcodes.AALOAD          => "AALOAD"
    case Opcodes.BALOAD          => "BALOAD"
    case Opcodes.CALOAD          => "CALOAD"
    case Opcodes.SALOAD          => "SALOAD"
    case Opcodes.IASTORE         => "IASTORE"
    case Opcodes.LASTORE         => "LASTORE"
    case Opcodes.FASTORE         => "FASTORE"
    case Opcodes.DASTORE         => "DASTORE"
    case Opcodes.AASTORE         => "AASTORE"
    case Opcodes.BASTORE         => "BASTORE"
    case Opcodes.CASTORE         => "CASTORE"
    case Opcodes.SASTORE         => "SASTORE"
    case Opcodes.IADD            => "IADD"
    case Opcodes.LADD            => "LADD"
    case Opcodes.FADD            => "FADD"
    case Opcodes.DADD            => "DADD"
    case Opcodes.ISUB            => "ISUB"
    case Opcodes.LSUB            => "LSUB"
    case Opcodes.FSUB            => "FSUB"
    case Opcodes.DSUB            => "DSUB"
    case Opcodes.IMUL            => "IMUL"
    case Opcodes.LMUL            => "LMUL"
    case Opcodes.FMUL            => "FMUL"
    case Opcodes.DMUL            => "DMUL"
    case Opcodes.IDIV            => "IDIV"
    case Opcodes.LDIV            => "LDIV"
    case Opcodes.FDIV            => "FDIV"
    case Opcodes.DDIV            => "DDIV"
    case Opcodes.IREM            => "IREM"
    case Opcodes.LREM            => "LREM"
    case Opcodes.FREM            => "FREM"
    case Opcodes.DREM            => "DREM"
    case Opcodes.INEG            => "INEG"
    case Opcodes.LNEG            => "LNEG"
    case Opcodes.FNEG            => "FNEG"
    case Opcodes.DNEG            => "DNEG"
    case Opcodes.ISHL            => "ISHL"
    case Opcodes.LSHL            => "LSHL"
    case Opcodes.ISHR            => "ISHR"
    case Opcodes.LSHR            => "LSHR"
    case Opcodes.IUSHR           => "IUSHR"
    case Opcodes.LUSHR           => "LUSHR"
    case Opcodes.IAND            => "IAND"
    case Opcodes.LAND            => "LAND"
    case Opcodes.IOR             => "IOR"
    case Opcodes.LOR             => "LOR"
    case Opcodes.IXOR            => "IXOR"
    case Opcodes.LXOR            => "LXOR"
    case Opcodes.IINC            => "IINC"
    case Opcodes.I2L             => "I2L"
    case Opcodes.I2F             => "I2F"
    case Opcodes.I2D             => "I2D"
    case Opcodes.L2I             => "L2I"
    case Opcodes.L2F             => "L2F"
    case Opcodes.L2D             => "L2D"
    case Opcodes.F2I             => "F2I"
    case Opcodes.F2L             => "F2L"
    case Opcodes.F2D             => "F2D"
    case Opcodes.D2I             => "D2I"
    case Opcodes.D2L             => "D2L"
    case Opcodes.D2F             => "D2F"
    case Opcodes.I2B             => "I2B"
    case Opcodes.I2C             => "I2C"
    case Opcodes.I2S             => "I2S"
    case Opcodes.LCMP            => "LCMP"
    case Opcodes.FCMPL           => "FCMPL"
    case Opcodes.FCMPG           => "FCMPG"
    case Opcodes.DCMPL           => "DCMPL"
    case Opcodes.DCMPG           => "DCMPG"
    case Opcodes.CHECKCAST       => "CHECKCAST"
    case Opcodes.INSTANCEOF      => "INSTANCEOF"
    case Opcodes.ARRAYLENGTH     => "ARRAYLENGTH"
    case Opcodes.ATHROW          => "ATHROW"
    case Opcodes.NEWARRAY        => "NEWARRAY"
    case Opcodes.ANEWARRAY       => "ANEWARRAY"
    case Opcodes.MULTIANEWARRAY  => "MULTIANEWARRAY"
    case _                       => s"OPCODE_$opcode"

  /** Converts access flags to a human-readable string.
    *
    * @param access
    *   The access flags bitmask
    * @return
    *   Space-separated access modifiers (e.g., "public static final")
    */
  def accessFlagsToString(access: Int): String =
    val flags = Seq(
      (Opcodes.ACC_PUBLIC, "public"),
      (Opcodes.ACC_PRIVATE, "private"),
      (Opcodes.ACC_PROTECTED, "protected"),
      (Opcodes.ACC_STATIC, "static"),
      (Opcodes.ACC_FINAL, "final"),
      (Opcodes.ACC_SYNCHRONIZED, "synchronized"),
      (Opcodes.ACC_VOLATILE, "volatile"),
      (Opcodes.ACC_TRANSIENT, "transient"),
      (Opcodes.ACC_ABSTRACT, "abstract"),
      (Opcodes.ACC_INTERFACE, "interface"),
      (Opcodes.ACC_NATIVE, "native"),
      (Opcodes.ACC_STRICT, "strictfp"),
      (Opcodes.ACC_SYNTHETIC, "synthetic"),
      (Opcodes.ACC_BRIDGE, "bridge"),
      (Opcodes.ACC_VARARGS, "varargs"),
      (Opcodes.ACC_ENUM, "enum"),
      (Opcodes.ACC_ANNOTATION, "annotation")
    ).collect { case (flag, name) if (access & flag) != 0 => name }

    if flags.isEmpty then "" else flags.mkString(" ")
