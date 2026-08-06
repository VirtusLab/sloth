package sloth.patching

import org.objectweb.asm.*
import org.objectweb.asm.tree.*
import org.objectweb.asm.Opcodes.*
import scala.jdk.CollectionConverters.*
import sloth.classfile.ClassfileParser
import sloth.lazyval.{LazyValDetector, LazyValInfo, LazyValDetectionResult, ScalaVersion, OffsetFieldLocation}
import sloth.analysis.ClassfileGroup

/** Patches Scala 3.x lazy val bytecode to Scala 3.8+ format.
  *
  * Supports multiple lazy val implementation variants:
  *   - Scala 3.0-3.2: Field-based with bitmap → VarHandle transformation (implemented)
  *   - Scala 3.3-3.7: Unsafe-based → VarHandle transformation (implemented)
  *
  * The 3.0-3.2 transformation:
  *   - Replaces bitmap-based inline lazy val initialization with VarHandle-based lzyINIT methods
  *   - 3.2 uses getDeclaredField+getOffsetStatic vs 3.0-3.1's getOffset, but the same patching logic handles both
  *
  * The 3.3-3.7 transformation:
  *   - Field level: OFFSET$_m_N (long) → <name>$lzyN$lzyHandle (VarHandle)
  *   - Static initializer: Unsafe offset lookup → VarHandle findVarHandle
  *   - lzyINIT methods: LazyVals$.objCAS → VarHandle.compareAndSet
  *   - Inner classes: Add MethodHandles$Lookup reference
  */
object BytecodePatcher {

  /** Result of a patching operation */
  sealed trait PatchResult
  object PatchResult {

    /** Single classfile was patched */
    case class PatchedSingle(name: String, bytes: Array[Byte]) extends PatchResult

    /** Companion pair was patched (both object and class) */
    case class PatchedPair(
        companionObjectName: String,
        className: String,
        companionObjectBytes: Array[Byte],
        classBytes: Array[Byte]
    ) extends PatchResult

    /** No patching needed (already 3.8+ or no lazy vals) */
    case object NotApplicable extends PatchResult

    /** Patching failed with error */
    final case class Failed(error: String) extends PatchResult
  }

  // String constants for scala.runtime.LazyVals types.
  // These must NOT appear as literal "scala/runtime/..." in the constant pool,
  // otherwise sbt-assembly shade rules (which rewrite "scala.**" class references)
  // will mangle them. These strings are used to match/generate bytecode for the
  // *application's* classes, not the agent's own shaded dependencies.
  //
  // We use Array(...).mkString to force runtime construction — simple string
  // concatenation gets constant-folded by the Scala compiler.
  private val ScalaRuntimePrefix: String = Array("sca", "la/run", "time/").mkString
  val LazyValsObj: String = ScalaRuntimePrefix + "LazyVals$"
  val LazyVals: String = ScalaRuntimePrefix + "LazyVals"
  val NullValue: String = ScalaRuntimePrefix + "LazyVals$NullValue$"
  val Evaluating: String = ScalaRuntimePrefix + "LazyVals$Evaluating$"
  val Waiting: String = ScalaRuntimePrefix + "LazyVals$Waiting"
  val ControlState: String = ScalaRuntimePrefix + "LazyVals$LazyValControlState"

  // Descriptors referencing LazyVals types (also need shade protection)
  val NullValueDesc: String = "L" + NullValue + ";"
  val EvaluatingDesc: String = "L" + Evaluating + ";"
  val LazyValsObjDesc: String = "L" + LazyValsObj + ";"

  /** Creates a ClassWriter, using the provided ClassLoader for class hierarchy resolution if available. */
  private def makeClassWriter(classLoader: Option[ClassLoader]): ClassWriter =
    classLoader match
      case Some(cl) => new ClassLoaderClassWriter(cl)
      case None     => new ClassWriter(ClassWriter.COMPUTE_FRAMES)

  /** Patches a classfile group to 3.8+ format, handling both single files and companion pairs.
    *
    * @param group
    *   The classfile group (single or companion pair)
    * @param classLoader
    *   Optional ClassLoader for resolving class hierarchies during frame computation
    * @return
    *   PatchResult indicating success, failure, or not applicable
    */
  def patch(group: ClassfileGroup, classLoader: Option[ClassLoader] = None): PatchResult = group match {
    case ClassfileGroup.Single(name, classInfo, bytes) =>
      // Detect lazy vals in single class (no companion)
      val detectionResult = LazyValDetector.detect(classInfo, None)

      val (lazyVals, version) = detectionResult match {
        case LazyValDetectionResult.NoLazyVals              => return PatchResult.NotApplicable
        case LazyValDetectionResult.LazyValsFound(lvs, ver) => (lvs, ver)
        case LazyValDetectionResult.MixedVersions(lvs) =>
          return PatchResult.Failed(buildDiagnostic("Mixed Scala versions detected", name, classInfo, lvs))
      }

      // Dispatch to version-specific patching
      version match {
        case ScalaVersion.Scala30x_31x => patchScala30x_31x(bytes, classInfo, lazyVals, name, classLoader = classLoader)
        case ScalaVersion.Scala32x     => patchScala32x(bytes, classInfo, lazyVals, name, classLoader = classLoader)
        case ScalaVersion.Scala33x_37x =>
          patchScala33x_37x(bytes, classInfo, lazyVals, name, None, None, classLoader = classLoader)
        case ScalaVersion.Scala38Plus => PatchResult.NotApplicable
        case ScalaVersion.Unknown(reason) =>
          PatchResult.Failed(buildDiagnostic(s"Unknown Scala version detected: $reason", name, classInfo, lazyVals))
      }

    case ClassfileGroup.CompanionPair(
          companionObjectName,
          className,
          companionObjectInfo,
          classInfo,
          companionObjectBytes,
          classBytes
        ) =>
      // Detect lazy vals in companion object with companion class context
      val objectDetectionResult = LazyValDetector.detect(companionObjectInfo, Some(classInfo))

      // Also detect lazy vals in companion class (standalone detection)
      val classDetectionResult = LazyValDetector.detect(classInfo, None)

      // Determine if we need to patch.
      //
      // A companion pair from a single Scala compiler can only have:
      //   1. One side with no lazy vals, the other with some version
      //   2. Both sides with the exact same version
      // Anything else is a bug in detection. After validating this invariant,
      // skip if the version is already Scala38Plus (genuine or already-patched bytes
      // fed back by a chained ClassFileTransformer).
      val (objectLazyVals, classLazyVals, version) = (objectDetectionResult, classDetectionResult) match {
        case (LazyValDetectionResult.NoLazyVals, LazyValDetectionResult.NoLazyVals) =>
          return PatchResult.NotApplicable
        case (LazyValDetectionResult.LazyValsFound(objLvs, objVer), LazyValDetectionResult.NoLazyVals) =>
          (objLvs, Seq.empty, objVer)
        case (LazyValDetectionResult.NoLazyVals, LazyValDetectionResult.LazyValsFound(clsLvs, clsVer)) =>
          (Seq.empty, clsLvs, clsVer)
        case (
              LazyValDetectionResult.LazyValsFound(objLvs, objVer),
              LazyValDetectionResult.LazyValsFound(clsLvs, clsVer)
            ) =>
          // Both have lazy vals — versions MUST match. A mismatch is always a bug.
          if (objVer != clsVer) {
            val diag = new StringBuilder
            diag.append(s"BUG: Companion class and object have different Scala versions: $clsVer vs $objVer\n")
            diag.append(s"\n--- Companion object: $companionObjectName (detected as $objVer) ---\n")
            diag.append(s"  Fields:\n")
            companionObjectInfo.fields.foreach { f =>
              diag.append(s"    ${f.name}:${f.descriptor} (access=0x${f.access.toHexString})\n")
            }
            diag.append(s"  Lazy vals (${objLvs.size}):\n")
            objLvs.foreach { lv =>
              diag.append(
                s"    ${lv.name} (index=${lv.index}, version=${lv.version}, offset=${lv.offsetField.map(_.name)}, varHandle=${lv.varHandleField
                    .map(_.name)}, bitmap=${lv.bitmapField.map(_.name)}, init=${lv.initMethod.map(_.name)})\n"
              )
            }
            diag.append(s"\n--- Companion class: $className (detected as $clsVer) ---\n")
            diag.append(s"  Fields:\n")
            classInfo.fields.foreach { f =>
              diag.append(s"    ${f.name}:${f.descriptor} (access=0x${f.access.toHexString})\n")
            }
            diag.append(s"  Lazy vals (${clsLvs.size}):\n")
            clsLvs.foreach { lv =>
              diag.append(
                s"    ${lv.name} (index=${lv.index}, version=${lv.version}, offset=${lv.offsetField.map(_.name)}, varHandle=${lv.varHandleField
                    .map(_.name)}, bitmap=${lv.bitmapField.map(_.name)}, init=${lv.initMethod.map(_.name)})\n"
              )
            }
            return PatchResult.Failed(diag.toString())
          }
          (objLvs, clsLvs, objVer)
        case (LazyValDetectionResult.MixedVersions(lvs), _) =>
          val allLvs = lvs ++ (classDetectionResult match {
            case LazyValDetectionResult.LazyValsFound(l, _) => l; case _ => Seq.empty
          })
          return PatchResult.Failed(
            buildDiagnostic(
              "Mixed Scala versions detected in companion object",
              companionObjectName,
              companionObjectInfo,
              allLvs
            )
          )
        case (_, LazyValDetectionResult.MixedVersions(lvs)) =>
          val allLvs = (objectDetectionResult match {
            case LazyValDetectionResult.LazyValsFound(l, _) => l; case _ => Seq.empty
          }) ++ lvs
          return PatchResult.Failed(
            buildDiagnostic("Mixed Scala versions detected in companion class", className, classInfo, allLvs)
          )
      }

      // Already in target format — nothing to patch
      if (version == ScalaVersion.Scala38Plus) {
        return PatchResult.NotApplicable
      }

      // Dispatch to version-specific patching based on what we found
      version match {
        case ScalaVersion.Scala30x_31x =>
          // Handle different companion pair scenarios for 3.0-3.1
          (objectLazyVals.nonEmpty, classLazyVals.nonEmpty) match {
            case (true, false) =>
              // Only object has lazy vals - companion class has OFFSET field
              patchScala30x_31x(
                companionObjectBytes,
                companionObjectInfo,
                objectLazyVals,
                companionObjectName,
                Some((className, classInfo, classBytes)),
                classLoader = classLoader
              )
            case (false, true) =>
              // Only class has lazy vals - patch as standalone
              patchScala30x_31x(classBytes, classInfo, classLazyVals, className, classLoader = classLoader)
            case (true, true) =>
              // BOTH have lazy vals - need to patch both independently
              patchCompanionPairBothHaveLazyVals30x_31x(
                companionObjectName,
                className,
                companionObjectInfo,
                classInfo,
                companionObjectBytes,
                classBytes,
                objectLazyVals,
                classLazyVals,
                classLoader = classLoader
              )
            case (false, false) =>
              PatchResult.NotApplicable
          }

        case ScalaVersion.Scala32x =>
          // Handle different companion pair scenarios for 3.2 (same logic as 3.0-3.1)
          (objectLazyVals.nonEmpty, classLazyVals.nonEmpty) match {
            case (true, false) =>
              // Only object has lazy vals - companion class has OFFSET field
              patchScala30x_31x(
                companionObjectBytes,
                companionObjectInfo,
                objectLazyVals,
                companionObjectName,
                Some((className, classInfo, classBytes)),
                classLoader = classLoader
              )
            case (false, true) =>
              // Only class has lazy vals - patch as standalone
              patchScala30x_31x(classBytes, classInfo, classLazyVals, className, classLoader = classLoader)
            case (true, true) =>
              // BOTH have lazy vals - need to patch both independently
              patchCompanionPairBothHaveLazyVals30x_31x(
                companionObjectName,
                className,
                companionObjectInfo,
                classInfo,
                companionObjectBytes,
                classBytes,
                objectLazyVals,
                classLazyVals,
                classLoader = classLoader
              )
            case (false, false) =>
              PatchResult.NotApplicable
          }

        case ScalaVersion.Scala33x_37x =>
          // Handle different companion pair scenarios
          (objectLazyVals.nonEmpty, classLazyVals.nonEmpty) match {
            case (true, false) =>
              // Only object has lazy vals
              val hasCompanionOffset =
                objectLazyVals.exists(_.offsetFieldLocation == OffsetFieldLocation.InCompanionClass)
              if (hasCompanionOffset) {
                patchScala33x_37x(
                  companionObjectBytes,
                  companionObjectInfo,
                  objectLazyVals,
                  companionObjectName,
                  Some((className, classInfo, classBytes)),
                  classLoader = classLoader
                )
              } else {
                patchScala33x_37x(
                  companionObjectBytes,
                  companionObjectInfo,
                  objectLazyVals,
                  companionObjectName,
                  None,
                  None,
                  classLoader = classLoader
                )
              }
            case (false, true) =>
              // Only class has lazy vals - patch as standalone
              patchScala33x_37x(classBytes, classInfo, classLazyVals, className, None, None, classLoader = classLoader)
            case (true, true) =>
              // BOTH have lazy vals - need to patch both independently
              patchCompanionPairBothHaveLazyVals33x_37x(
                companionObjectName,
                className,
                companionObjectInfo,
                classInfo,
                companionObjectBytes,
                classBytes,
                objectLazyVals,
                classLazyVals,
                classLoader = classLoader
              )
            case (false, false) =>
              PatchResult.NotApplicable
          }

        case ScalaVersion.Scala38Plus => PatchResult.NotApplicable
        case ScalaVersion.Unknown(reason) =>
          val allLvs = objectLazyVals ++ classLazyVals
          PatchResult.Failed(
            buildDiagnostic(
              s"Unknown Scala version detected: $reason",
              companionObjectName,
              companionObjectInfo,
              allLvs
            )
          )
      }
  }

  /** Builds a multi-line diagnostic message for Failed results. */
  private def buildDiagnostic(
      headline: String,
      className: String,
      classInfo: sloth.classfile.ClassInfo,
      lazyVals: Seq[LazyValInfo]
  ): String = {
    val sb = new StringBuilder
    sb.append(headline).append("\n")
    sb.append(s"  Class: $className\n")
    sb.append(s"  Fields:\n")
    classInfo.fields.foreach { f =>
      sb.append(s"    ${f.name}:${f.descriptor} (access=0x${f.access.toHexString})\n")
    }
    sb.append(s"  Methods:\n")
    classInfo.methods.foreach { m =>
      sb.append(s"    ${m.name}:${m.descriptor}\n")
    }
    sb.append(s"  Lazy vals (${lazyVals.size}):\n")
    lazyVals.foreach { lv =>
      val versionStr = lv.version match {
        case ScalaVersion.Unknown(reason) => s"Unknown($reason)"
        case other                        => other.toString
      }
      sb.append(s"    ${lv.name} (index=${lv.index}, version=$versionStr)\n")
    }
    sb.toString()
  }

  // ============================================================================
  // Scala 3.0-3.1 Patching Strategy (Field-based with bitmap)
  // ============================================================================

  /** Patches Scala 3.0-3.1 lazy vals to 3.8+ format.
    *
    * Transforms bitmap-based lazy vals with inline initialization to VarHandle-based implementation with separate
    * lzyINIT methods, matching the 3.8+ pattern.
    *
    * @param companionInfo
    *   Optional tuple of (className, classInfo, classBytes) for companion class that contains OFFSET fields
    */
  private def patchScala30x_31x(
      bytes: Array[Byte],
      classInfo: sloth.classfile.ClassInfo,
      lazyVals: Seq[LazyValInfo],
      name: String,
      companionInfo: Option[(String, sloth.classfile.ClassInfo, Array[Byte])] = None,
      classLoader: Option[ClassLoader] = None
  ): PatchResult = {
    try {
      companionInfo match {
        case Some((companionClassName, companionClassInfo, companionClassBytes)) =>
          // Companion object with OFFSET fields in companion class
          // 1. Patch companion class: remove OFFSET field and its clinit init
          val companionReader = new ClassReader(companionClassBytes)
          val companionClassNode = new ClassNode(ASM9)
          companionReader.accept(companionClassNode, ClassReader.EXPAND_FRAMES)

          patchCompanionClass30x_31x(companionClassNode)

          val companionWriter = makeClassWriter(classLoader)
          companionClassNode.accept(companionWriter)
          val patchedClassBytes = companionWriter.toByteArray

          // 2. Patch companion object: full transformation
          val objectReader = new ClassReader(bytes)
          val objectNode = new ClassNode(ASM9)
          objectReader.accept(objectNode, ClassReader.EXPAND_FRAMES)

          patchClassNode30x_31x(objectNode, classInfo.name, lazyVals)

          val objectWriter = makeClassWriter(classLoader)
          objectNode.accept(objectWriter)
          val patchedObjectBytes = objectWriter.toByteArray

          PatchResult.PatchedPair(name, companionClassName, patchedObjectBytes, patchedClassBytes)

        case None =>
          // Standalone class/object
          val reader = new ClassReader(bytes)
          val classNode = new ClassNode(ASM9)
          reader.accept(classNode, ClassReader.EXPAND_FRAMES)

          patchClassNode30x_31x(classNode, classInfo.name, lazyVals)

          val writer = makeClassWriter(classLoader)
          classNode.accept(writer)
          val patchedBytes = writer.toByteArray

          PatchResult.PatchedSingle(name, patchedBytes)
      }
    } catch {
      case e: Exception =>
        e.printStackTrace()
        PatchResult.Failed(s"Patching failed: ${e.getMessage}")
    }
  }

  /** Patches companion pair where BOTH class and object have lazy vals (3.0-3.1). */
  private def patchCompanionPairBothHaveLazyVals30x_31x(
      companionObjectName: String,
      className: String,
      companionObjectInfo: sloth.classfile.ClassInfo,
      classInfo: sloth.classfile.ClassInfo,
      companionObjectBytes: Array[Byte],
      classBytes: Array[Byte],
      objectLazyVals: Seq[LazyValInfo],
      classLazyVals: Seq[LazyValInfo],
      classLoader: Option[ClassLoader] = None
  ): PatchResult = {
    try {
      // Step 1: Patch object (which also patches class to remove OFFSET fields)
      val objectPatchResult = patchScala30x_31x(
        companionObjectBytes,
        companionObjectInfo,
        objectLazyVals,
        companionObjectName,
        Some((className, classInfo, classBytes)),
        classLoader = classLoader
      )

      objectPatchResult match {
        case PatchResult.PatchedPair(_, _, patchedObjectBytes, intermediatePatchedClassBytes) =>
          // Step 2: Parse the intermediate patched class bytes and patch again for class's own lazy vals
          val intermediateClassInfo = ClassfileParser.parse(intermediatePatchedClassBytes).toOption.get
          val reader = new ClassReader(intermediatePatchedClassBytes)
          val classNode = new ClassNode(ASM9)
          reader.accept(classNode, ClassReader.EXPAND_FRAMES)

          patchClassNode30x_31x(classNode, className, classLazyVals)

          val writer = makeClassWriter(classLoader)
          classNode.accept(writer)
          val finalPatchedClassBytes = writer.toByteArray

          PatchResult.PatchedPair(companionObjectName, className, patchedObjectBytes, finalPatchedClassBytes)

        case PatchResult.Failed(err) =>
          PatchResult.Failed(s"Failed to patch companion object: $err")
        case _ =>
          PatchResult.Failed("Unexpected result when patching companion object")
      }
    } catch {
      case e: Exception =>
        e.printStackTrace()
        PatchResult.Failed(s"Patching failed for companion pair: ${e.getMessage}")
    }
  }

  /** Patches companion class for 3.0-3.1: removes OFFSET fields and their clinit initialization. */
  private def patchCompanionClass30x_31x(classNode: ClassNode): Unit = {
    // Remove OFFSET fields (OFFSET$_m_N pattern)
    val fieldsToRemove = classNode.fields.asScala.filter { field =>
      field.desc == "J" && field.name.matches("OFFSET\\$_m_\\d+")
    }.toSeq
    fieldsToRemove.foreach(classNode.fields.remove)

    // Remove OFFSET initialization from <clinit>
    classNode.methods.asScala.find(_.name == "<clinit>").foreach { clinit =>
      val instructions = clinit.instructions
      val toRemove = scala.collection.mutable.ListBuffer[AbstractInsnNode]()

      var current = instructions.getFirst
      while (current != null) {
        current match {
          case fieldInsn: FieldInsnNode
              if fieldInsn.getOpcode == PUTSTATIC &&
                fieldInsn.desc == "J" &&
                fieldInsn.name.matches("OFFSET\\$_m_\\d+") =>
            // Found PUTSTATIC for OFFSET$_m_N. Walk backwards to find GETSTATIC LazyVals$.MODULE$
            var prev = current.getPrevious
            var removeCount = 0
            while (prev != null && removeCount < 10) {
              prev match {
                case gs: FieldInsnNode
                    if gs.getOpcode == GETSTATIC &&
                      gs.owner == LazyValsObj && gs.name == "MODULE$" =>
                  var temp = prev
                  while (temp != null && temp != current.getNext) {
                    toRemove += temp
                    temp = temp.getNext
                  }
                  removeCount = 999
                  prev = null
                case _ =>
                  removeCount += 1
                  prev = prev.getPrevious
              }
            }
          case _ =>
        }
        current = current.getNext
      }

      toRemove.foreach(instructions.remove)
    }
  }

  /** Core patching logic for Scala 3.0-3.1 lazy vals.
    *
    * Transforms bitmap-based inline initialization to VarHandle-based with separate lzyINIT methods.
    */
  private def patchClassNode30x_31x(classNode: ClassNode, className: String, lazyVals: Seq[LazyValInfo]): Unit = {
    val classInternalName = className.replace('.', '/')

    // Step 1: Field transformations
    // Remove OFFSET fields
    val offsetFields = classNode.fields.asScala.filter { field =>
      field.desc == "J" && (field.name.matches("OFFSET\\$_m_\\d+") || field.name.matches("OFFSET\\$\\d+"))
    }.toSeq
    offsetFields.foreach(classNode.fields.remove)

    // Remove bitmap fields
    val bitmapFields = classNode.fields.asScala.filter { field =>
      field.desc == "J" && field.name.matches("\\d+bitmap\\$\\d+")
    }.toSeq
    bitmapFields.foreach(classNode.fields.remove)

    // Collect original storage field descriptors before modifying
    val storageFieldDescriptors = lazyVals.map { lv =>
      val storageFieldName = s"${lv.name}$$lzy${lv.index}"
      val field = classNode.fields.asScala.find(_.name == storageFieldName)
      val originalDesc = field.map(_.desc).getOrElse(lv.storageField.descriptor)
      (lv, storageFieldName, originalDesc)
    }

    // Transform storage fields: change to private volatile Object, remove ACC_STATIC
    storageFieldDescriptors.foreach { case (lv, storageFieldName, _) =>
      classNode.fields.asScala.find(_.name == storageFieldName).foreach { field =>
        field.desc = "Ljava/lang/Object;"
        field.access = ACC_PRIVATE | ACC_VOLATILE
      }
    }

    // Add VarHandle fields
    storageFieldDescriptors.foreach { case (lv, storageFieldName, _) =>
      val varHandleName = s"${storageFieldName}$$lzyHandle"
      if (!classNode.fields.asScala.exists(_.name == varHandleName)) {
        classNode.fields.add(
          new FieldNode(
            ASM9,
            ACC_PRIVATE | ACC_STATIC | ACC_FINAL,
            varHandleName,
            "Ljava/lang/invoke/VarHandle;",
            null,
            null
          )
        )
      }
    }

    // Step 2: Replace <clinit>
    val clinitOpt = classNode.methods.asScala.find(_.name == "<clinit>")
    clinitOpt match {
      case Some(clinit) =>
        patchClinit30x_31x(clinit, classInternalName, storageFieldDescriptors)
      case None =>
        // Create new <clinit>
        val clinit = new MethodNode(ASM9, ACC_STATIC, "<clinit>", "()V", null, null)
        addVarHandleInits(clinit.instructions, classInternalName, storageFieldDescriptors)
        clinit.instructions.add(new InsnNode(RETURN))
        classNode.methods.add(clinit)
    }

    // Step 3: For each lazy val, extract computation, replace accessor, generate lzyINIT
    storageFieldDescriptors.foreach { case (lv, storageFieldName, originalDesc) =>
      val accessorName = lv.name
      val varHandleName = s"${storageFieldName}$$lzyHandle"
      val lzyInitName = s"${lv.name}$$lzyINIT${lv.index}"

      // Find the old accessor method
      classNode.methods.asScala.find(_.name == accessorName).foreach { accessor =>
        // Extract computation instructions from the old accessor
        val computation = extractComputation30x_31x(accessor)

        // Replace accessor body with 3.8+ pattern
        replaceAccessor30x_31x(accessor, classInternalName, storageFieldName, varHandleName, lzyInitName, originalDesc)

        // Generate lzyINIT method
        val lzyInit = generateLzyInit30x_31x(
          classInternalName,
          storageFieldName,
          varHandleName,
          lzyInitName,
          originalDesc,
          computation
        )
        classNode.methods.add(lzyInit)
      }
    }

    // Step 4: Add inner class references for 3.8+ lazy val types
    addLazyValInnerClasses(classNode)
  }

  /** Extracts the computation instructions from a 3.0-3.1 accessor method.
    *
    * In 3.0-3.1 bytecode, the computation is between the CAS IFEQ and either:
    *   - The first store to slot 5 (xSTORE 5) for normal computations
    *   - The end of the try-catch block (for computations that always throw)
    *
    * Returns cloned instructions with a proper label map so that JumpInsnNodes work correctly.
    */
  private def extractComputation30x_31x(accessor: MethodNode): Seq[AbstractInsnNode] = {
    val instructions = accessor.instructions

    // Find the CAS call
    var current = instructions.getFirst
    var casNode: MethodInsnNode = null
    while (current != null && casNode == null) {
      current match {
        case m: MethodInsnNode if m.owner == LazyValsObj && m.name == "CAS" =>
          casNode = m
        case _ =>
      }
      current = current.getNext
    }

    if (casNode == null) return Seq.empty

    // After CAS: IFEQ target
    val ifeq = casNode.getNext
    if (ifeq == null || !ifeq.isInstanceOf[JumpInsnNode]) return Seq.empty

    // Find the try-catch block that covers the computation (handler calls setFlag)
    // This gives us the boundary for always-throwing computations
    val tryCatchEndLabels: Set[LabelNode] = accessor.tryCatchBlocks.asScala.collect {
      case tcb if {
            // Check if handler code calls setFlag (indicating it's the lazy val exception handler)
            var n = tcb.handler.getNext
            var isSetFlagHandler = false
            var checked = 0
            while (n != null && checked < 10 && !isSetFlagHandler) {
              n match {
                case m: MethodInsnNode if m.owner == LazyValsObj && m.name == "setFlag" =>
                  isSetFlagHandler = true
                case _ =>
              }
              n = n.getNext
              checked += 1
            }
            isSetFlagHandler
          } =>
        tcb.end
    }.toSet

    // First pass: collect raw instructions (including labels) up to xSTORE 5 or try-catch end
    val rawInsns = scala.collection.mutable.ArrayBuffer[AbstractInsnNode]()
    current = ifeq.getNext
    var done = false
    while (current != null && !done) {
      current match {
        case v: VarInsnNode if v.`var` == 5 && isStoreOpcode(v.getOpcode) =>
          done = true
        case l: LabelNode if tryCatchEndLabels.contains(l) =>
          // Reached the end of the computation try-catch block (always-throwing case)
          done = true
        case _ =>
          rawInsns += current
          current = current.getNext
      }
    }

    // Build label clone map from all labels in the extracted range
    val labelMap = new java.util.HashMap[LabelNode, LabelNode]()
    rawInsns.foreach {
      case l: LabelNode => labelMap.put(l, new LabelNode())
      case _            =>
    }

    // Second pass: clone instructions (skip FrameNodes which are recomputed by COMPUTE_FRAMES)
    rawInsns.flatMap {
      case _: FrameNode => None
      case insn         => Some(insn.clone(labelMap))
    }.toSeq
  }

  /** Checks if an opcode is a store instruction. */
  private def isStoreOpcode(opcode: Int): Boolean =
    opcode == ISTORE || opcode == LSTORE || opcode == FSTORE || opcode == DSTORE || opcode == ASTORE

  /** Replaces a 3.0-3.1 accessor body with the 3.8+ accessor pattern. */
  private def replaceAccessor30x_31x(
      accessor: MethodNode,
      classInternalName: String,
      storageFieldName: String,
      varHandleName: String,
      lzyInitName: String,
      originalDesc: String
  ): Unit = {
    val typeInfo = getTypeInfo(originalDesc)
    accessor.instructions.clear()
    accessor.tryCatchBlocks.clear()
    accessor.localVariables = new java.util.ArrayList()
    accessor.maxStack = 2
    accessor.maxLocals = 2

    val insns = accessor.instructions
    val lNullCheck = new LabelNode()
    val lInit = new LabelNode()

    // ALOAD 0
    insns.add(new VarInsnNode(ALOAD, 0))
    // GETFIELD storage
    insns.add(new FieldInsnNode(GETFIELD, classInternalName, storageFieldName, "Ljava/lang/Object;"))
    // ASTORE 1
    insns.add(new VarInsnNode(ASTORE, 1))
    // ALOAD 1
    insns.add(new VarInsnNode(ALOAD, 1))
    // INSTANCEOF <type>
    insns.add(new TypeInsnNode(INSTANCEOF, typeInfo.instanceOfType))
    // IFEQ lNullCheck
    insns.add(new JumpInsnNode(IFEQ, lNullCheck))
    // ALOAD 1
    insns.add(new VarInsnNode(ALOAD, 1))
    // unbox/checkcast + return
    typeInfo.addUnboxOrCast(insns)
    insns.add(new InsnNode(typeInfo.returnOpcode))

    // lNullCheck:
    insns.add(lNullCheck)
    // ALOAD 1
    insns.add(new VarInsnNode(ALOAD, 1))
    // GETSTATIC LazyVals$NullValue$.MODULE$
    insns.add(new FieldInsnNode(GETSTATIC, NullValue, "MODULE$", NullValueDesc))
    // IF_ACMPNE lInit
    insns.add(new JumpInsnNode(IF_ACMPNE, lInit))

    if (typeInfo.isPrimitive) {
      // ACONST_NULL + unbox + return (for primitives, unboxToInt(null) returns 0)
      insns.add(new InsnNode(ACONST_NULL))
      typeInfo.addUnboxOrCast(insns)
      insns.add(new InsnNode(typeInfo.returnOpcode))
    } else {
      // ACONST_NULL + ARETURN
      insns.add(new InsnNode(ACONST_NULL))
      insns.add(new InsnNode(ARETURN))
    }

    // lInit:
    insns.add(lInit)
    // ALOAD 0
    insns.add(new VarInsnNode(ALOAD, 0))
    // INVOKESPECIAL lzyINIT
    insns.add(new MethodInsnNode(INVOKESPECIAL, classInternalName, lzyInitName, "()Ljava/lang/Object;", false))
    // unbox/checkcast + return
    typeInfo.addUnboxOrCast(insns)
    insns.add(new InsnNode(typeInfo.returnOpcode))
  }

  /** Generates a 3.8+ lzyINIT method with embedded computation from old 3.0-3.1 accessor.
    *
    * Follows the exact 3.8+ pattern: CAS loop with Evaluating$/Waiting/NullValue$ sentinels.
    */
  private def generateLzyInit30x_31x(
      classInternalName: String,
      storageFieldName: String,
      varHandleName: String,
      lzyInitName: String,
      originalDesc: String,
      computation: Seq[AbstractInsnNode]
  ): MethodNode = {
    val typeInfo = getTypeInfo(originalDesc)
    val method = new MethodNode(ASM9, ACC_PRIVATE, lzyInitName, "()Ljava/lang/Object;", null, null)
    val insns = method.instructions

    // Labels
    val lNonNull = new LabelNode() // jump target when value is non-null
    val lCasFailed = new LabelNode() // CAS null→Evaluating failed, goto 0
    val lNullMapping = new LabelNode() // null→NullValue$ mapping
    val lAfterCompute = new LabelNode() // after computation block
    val lExHandler = new LabelNode() // exception handler
    val lExCasOk = new LabelNode() // in exception handler, CAS succeeded
    val lSuccessCasOk = new LabelNode() // in success path, CAS succeeded
    val lReturnResult = new LabelNode() // return aload_3
    val lLoopBack = new LabelNode() // goto 0 (loop back)
    val lCheckEvaluating = new LabelNode() // check if Evaluating$
    val lCheckWaiting = new LabelNode() // check if Waiting
    val lReturnNull = new LabelNode() // return null (unknown LazyValControlState)
    val lReturnVal = new LabelNode() // return non-control-state value

    // Start of method (offset 0) — the loop target
    val lStart = new LabelNode()
    insns.add(lStart)

    // ALOAD 0; GETFIELD storage; ASTORE 1
    insns.add(new VarInsnNode(ALOAD, 0))
    insns.add(new FieldInsnNode(GETFIELD, classInternalName, storageFieldName, "Ljava/lang/Object;"))
    insns.add(new VarInsnNode(ASTORE, 1))

    // ALOAD 1; IFNONNULL lNonNull
    insns.add(new VarInsnNode(ALOAD, 1))
    insns.add(new JumpInsnNode(IFNONNULL, lNonNull))

    // CAS null → Evaluating$
    insns.add(new FieldInsnNode(GETSTATIC, classInternalName, varHandleName, "Ljava/lang/invoke/VarHandle;"))
    insns.add(new VarInsnNode(ALOAD, 0))
    insns.add(new InsnNode(ACONST_NULL))
    insns.add(new FieldInsnNode(GETSTATIC, Evaluating, "MODULE$", EvaluatingDesc))
    insns.add(
      new MethodInsnNode(
        INVOKEVIRTUAL,
        "java/lang/invoke/VarHandle",
        "compareAndSet",
        "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Z",
        false
      )
    )
    insns.add(new JumpInsnNode(IFEQ, lCasFailed))

    // CAS succeeded: compute value
    // ACONST_NULL; ASTORE 2 (result holder)
    insns.add(new InsnNode(ACONST_NULL))
    insns.add(new VarInsnNode(ASTORE, 2))
    // ACONST_NULL; ASTORE 3 (computation result for return)
    insns.add(new InsnNode(ACONST_NULL))
    insns.add(new VarInsnNode(ASTORE, 3))

    // Try block starts here
    val lTryStart = new LabelNode()
    insns.add(lTryStart)

    // Embedded computation → box if needed → ASTORE 3
    // Instructions are already cloned during extraction
    computation.foreach(insn => insns.add(insn))
    if (typeInfo.isPrimitive) {
      typeInfo.addBox(insns)
    }
    insns.add(new VarInsnNode(ASTORE, 3))

    // null → NullValue$ mapping: ALOAD 3; IFNONNULL lNullMapping
    insns.add(new VarInsnNode(ALOAD, 3))
    insns.add(new JumpInsnNode(IFNONNULL, lNullMapping))
    insns.add(new FieldInsnNode(GETSTATIC, NullValue, "MODULE$", NullValueDesc))
    insns.add(new VarInsnNode(ASTORE, 2))
    insns.add(new JumpInsnNode(GOTO, lAfterCompute))

    // lNullMapping: value was non-null
    insns.add(lNullMapping)
    insns.add(new VarInsnNode(ALOAD, 3))
    insns.add(new VarInsnNode(ASTORE, 2))

    // lAfterCompute: success path — CAS Evaluating$ → result
    insns.add(lAfterCompute)
    insns.add(new JumpInsnNode(GOTO, lSuccessCasOk))

    // Exception handler
    insns.add(lExHandler)
    insns.add(new VarInsnNode(ASTORE, 4))

    // CAS Evaluating$ → result (in exception handler, result is still in slot 2)
    insns.add(new FieldInsnNode(GETSTATIC, classInternalName, varHandleName, "Ljava/lang/invoke/VarHandle;"))
    insns.add(new VarInsnNode(ALOAD, 0))
    insns.add(new FieldInsnNode(GETSTATIC, Evaluating, "MODULE$", EvaluatingDesc))
    insns.add(new VarInsnNode(ALOAD, 2))
    insns.add(
      new MethodInsnNode(
        INVOKEVIRTUAL,
        "java/lang/invoke/VarHandle",
        "compareAndSet",
        "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Z",
        false
      )
    )
    insns.add(new JumpInsnNode(IFNE, lExCasOk))

    // Exception handler: CAS failed, need to countDown Waiting
    insns.add(new VarInsnNode(ALOAD, 0))
    insns.add(new FieldInsnNode(GETFIELD, classInternalName, storageFieldName, "Ljava/lang/Object;"))
    insns.add(new TypeInsnNode(CHECKCAST, Waiting))
    insns.add(new VarInsnNode(ASTORE, 5))
    insns.add(new FieldInsnNode(GETSTATIC, classInternalName, varHandleName, "Ljava/lang/invoke/VarHandle;"))
    insns.add(new VarInsnNode(ALOAD, 0))
    insns.add(new VarInsnNode(ALOAD, 5))
    insns.add(new VarInsnNode(ALOAD, 2))
    insns.add(
      new MethodInsnNode(
        INVOKEVIRTUAL,
        "java/lang/invoke/VarHandle",
        "compareAndSet",
        "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Z",
        false
      )
    )
    insns.add(new InsnNode(POP))
    insns.add(new VarInsnNode(ALOAD, 5))
    insns.add(new MethodInsnNode(INVOKEVIRTUAL, Waiting, "countDown", "()V", false))

    // lExCasOk: rethrow exception
    insns.add(lExCasOk)
    insns.add(new VarInsnNode(ALOAD, 4))
    insns.add(new InsnNode(ATHROW))

    // Success path: CAS Evaluating$ → result
    val lSuccessStart = new LabelNode()
    insns.add(lSuccessStart)
    insns.add(lSuccessCasOk)
    insns.add(new FieldInsnNode(GETSTATIC, classInternalName, varHandleName, "Ljava/lang/invoke/VarHandle;"))
    insns.add(new VarInsnNode(ALOAD, 0))
    insns.add(new FieldInsnNode(GETSTATIC, Evaluating, "MODULE$", EvaluatingDesc))
    insns.add(new VarInsnNode(ALOAD, 2))
    insns.add(
      new MethodInsnNode(
        INVOKEVIRTUAL,
        "java/lang/invoke/VarHandle",
        "compareAndSet",
        "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Z",
        false
      )
    )
    insns.add(new JumpInsnNode(IFNE, lReturnResult))

    // Success CAS failed: countDown Waiting and return
    insns.add(new VarInsnNode(ALOAD, 0))
    insns.add(new FieldInsnNode(GETFIELD, classInternalName, storageFieldName, "Ljava/lang/Object;"))
    insns.add(new TypeInsnNode(CHECKCAST, Waiting))
    insns.add(new VarInsnNode(ASTORE, 5))
    insns.add(new FieldInsnNode(GETSTATIC, classInternalName, varHandleName, "Ljava/lang/invoke/VarHandle;"))
    insns.add(new VarInsnNode(ALOAD, 0))
    insns.add(new VarInsnNode(ALOAD, 5))
    insns.add(new VarInsnNode(ALOAD, 2))
    insns.add(
      new MethodInsnNode(
        INVOKEVIRTUAL,
        "java/lang/invoke/VarHandle",
        "compareAndSet",
        "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Z",
        false
      )
    )
    insns.add(new InsnNode(POP))
    insns.add(new VarInsnNode(ALOAD, 5))
    insns.add(new MethodInsnNode(INVOKEVIRTUAL, Waiting, "countDown", "()V", false))

    // lReturnResult: return slot 3
    insns.add(lReturnResult)
    insns.add(new VarInsnNode(ALOAD, 3))
    insns.add(new InsnNode(ARETURN))

    // lCasFailed: loop back
    insns.add(lCasFailed)
    insns.add(new JumpInsnNode(GOTO, lStart))

    // lNonNull: check if LazyValControlState
    insns.add(lNonNull)
    insns.add(new VarInsnNode(ALOAD, 1))
    insns.add(new TypeInsnNode(INSTANCEOF, ControlState))
    insns.add(new JumpInsnNode(IFEQ, lReturnVal))

    // Check Evaluating$
    insns.add(new VarInsnNode(ALOAD, 1))
    insns.add(new FieldInsnNode(GETSTATIC, Evaluating, "MODULE$", EvaluatingDesc))
    insns.add(new JumpInsnNode(IF_ACMPNE, lCheckWaiting))

    // Is Evaluating$: CAS Evaluating$ → new Waiting, loop back
    insns.add(new FieldInsnNode(GETSTATIC, classInternalName, varHandleName, "Ljava/lang/invoke/VarHandle;"))
    insns.add(new VarInsnNode(ALOAD, 0))
    insns.add(new VarInsnNode(ALOAD, 1))
    insns.add(new TypeInsnNode(NEW, Waiting))
    insns.add(new InsnNode(DUP))
    insns.add(new MethodInsnNode(INVOKESPECIAL, Waiting, "<init>", "()V", false))
    insns.add(
      new MethodInsnNode(
        INVOKEVIRTUAL,
        "java/lang/invoke/VarHandle",
        "compareAndSet",
        "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Z",
        false
      )
    )
    insns.add(new InsnNode(POP))
    insns.add(new JumpInsnNode(GOTO, lStart))

    // lCheckWaiting: check Waiting
    insns.add(lCheckWaiting)
    insns.add(new VarInsnNode(ALOAD, 1))
    insns.add(new TypeInsnNode(INSTANCEOF, Waiting))
    insns.add(new JumpInsnNode(IFEQ, lReturnNull))

    // Is Waiting: await and loop back
    insns.add(new VarInsnNode(ALOAD, 1))
    insns.add(new TypeInsnNode(CHECKCAST, Waiting))
    insns.add(new MethodInsnNode(INVOKEVIRTUAL, Waiting, "await", "()V", false))
    insns.add(new JumpInsnNode(GOTO, lStart))

    // lReturnNull: unknown control state, return null
    insns.add(lReturnNull)
    insns.add(new InsnNode(ACONST_NULL))
    insns.add(new InsnNode(ARETURN))

    // lReturnVal: not a control state, return the value
    insns.add(lReturnVal)
    insns.add(new VarInsnNode(ALOAD, 1))
    insns.add(new InsnNode(ARETURN))

    // Exception table: try block covers computation
    method.tryCatchBlocks.add(new TryCatchBlockNode(lTryStart, lExHandler, lExHandler, null))

    method.maxStack = 5
    method.maxLocals = 6

    method
  }

  /** Patches <clinit> for 3.0-3.1: removes OFFSET initialization, adds VarHandle initialization. Preserves MODULE$
    * initialization for objects.
    */
  private def patchClinit30x_31x(
      clinit: MethodNode,
      classInternalName: String,
      storageFieldDescriptors: Seq[(LazyValInfo, String, String)]
  ): Unit = {
    val instructions = clinit.instructions

    // Remove entire getOffset sequence: GETSTATIC LazyVals$.MODULE$ ... PUTSTATIC OFFSET$*
    val toRemove = scala.collection.mutable.ArrayBuffer[AbstractInsnNode]()

    var current = instructions.getFirst
    while (current != null) {
      current match {
        case putStatic: FieldInsnNode
            if putStatic.getOpcode == PUTSTATIC &&
              putStatic.desc == "J" &&
              (putStatic.name.matches("OFFSET\\$_m_\\d+") || putStatic.name.matches("OFFSET\\$\\d+")) =>
          // Walk backwards to find GETSTATIC LazyVals$.MODULE$
          var prev = current.getPrevious
          var count = 0
          while (prev != null && count < 10) {
            prev match {
              case gs: FieldInsnNode
                  if gs.getOpcode == GETSTATIC &&
                    gs.owner == LazyValsObj && gs.name == "MODULE$" =>
                // Found start, collect everything from here to putStatic
                var temp = prev
                while (temp != null && temp != current.getNext) {
                  toRemove += temp
                  temp = temp.getNext
                }
                count = 999
                prev = null
              case _ =>
                count += 1
                prev = prev.getPrevious
            }
          }
        case _ =>
      }
      current = current.getNext
    }
    toRemove.foreach(instructions.remove)

    // Insert VarHandle initialization at the beginning (before MODULE$ init for objects)
    val firstInsn = instructions.getFirst
    if (firstInsn != null) {
      val initInsns = new InsnList()
      addVarHandleInits(initInsns, classInternalName, storageFieldDescriptors)
      instructions.insertBefore(firstInsn, initInsns)
    } else {
      addVarHandleInits(instructions, classInternalName, storageFieldDescriptors)
      instructions.add(new InsnNode(RETURN))
    }
  }

  /** Adds VarHandle field initialization instructions. */
  private def addVarHandleInits(
      insns: InsnList,
      classInternalName: String,
      storageFieldDescriptors: Seq[(LazyValInfo, String, String)]
  ): Unit = {
    storageFieldDescriptors.foreach { case (lv, storageFieldName, _) =>
      val varHandleName = s"${storageFieldName}$$lzyHandle"

      // MethodHandles.lookup()
      insns.add(
        new MethodInsnNode(
          INVOKESTATIC,
          "java/lang/invoke/MethodHandles",
          "lookup",
          "()Ljava/lang/invoke/MethodHandles$Lookup;",
          false
        )
      )
      // LDC <Class>
      insns.add(new LdcInsnNode(org.objectweb.asm.Type.getObjectType(classInternalName)))
      // LDC <FieldName>
      insns.add(new LdcInsnNode(storageFieldName))
      // LDC Object.class
      insns.add(new LdcInsnNode(org.objectweb.asm.Type.getType("Ljava/lang/Object;")))
      // findVarHandle
      insns.add(
        new MethodInsnNode(
          INVOKEVIRTUAL,
          "java/lang/invoke/MethodHandles$Lookup",
          "findVarHandle",
          "(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;)Ljava/lang/invoke/VarHandle;",
          false
        )
      )
      // PUTSTATIC
      insns.add(new FieldInsnNode(PUTSTATIC, classInternalName, varHandleName, "Ljava/lang/invoke/VarHandle;"))
    }
  }

  /** Adds inner class references required for 3.8+ lazy val implementation. */
  private def addLazyValInnerClasses(classNode: ClassNode): Unit = {
    val innerClassesToAdd = Seq(
      (
        "java/lang/invoke/MethodHandles$Lookup",
        "java/lang/invoke/MethodHandles",
        "Lookup",
        ACC_PUBLIC | ACC_FINAL | ACC_STATIC
      ),
      (Evaluating, LazyVals, "Evaluating$", ACC_PUBLIC | ACC_FINAL | ACC_STATIC),
      (ControlState, LazyVals, "LazyValControlState", ACC_PUBLIC | ACC_STATIC),
      (NullValue, LazyVals, "NullValue$", ACC_PUBLIC | ACC_FINAL | ACC_STATIC),
      (Waiting, LazyVals, "Waiting", ACC_PUBLIC | ACC_FINAL | ACC_STATIC)
    )

    innerClassesToAdd.foreach { case (name, outerName, innerName, access) =>
      val exists = classNode.innerClasses.asScala.exists(_.name == name)
      if (!exists) {
        classNode.innerClasses.add(new InnerClassNode(name, outerName, innerName, access))
      }
    }
  }

  /** Type information for boxing/unboxing in 3.8+ lazy val patterns. */
  private case class TypeInfo(
      instanceOfType: String,
      isPrimitive: Boolean,
      returnOpcode: Int,
      addUnboxOrCast: InsnList => Unit,
      addBox: InsnList => Unit
  )

  /** Gets type information for boxing/unboxing based on original field descriptor. */
  private def getTypeInfo(descriptor: String): TypeInfo = descriptor match {
    case "I" =>
      TypeInfo(
        "java/lang/Integer",
        true,
        IRETURN,
        insns =>
          insns.add(
            new MethodInsnNode(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", false)
          ),
        insns =>
          insns.add(
            new MethodInsnNode(
              INVOKESTATIC,
              "scala/runtime/BoxesRunTime",
              "boxToInteger",
              "(I)Ljava/lang/Integer;",
              false
            )
          )
      )
    case "J" =>
      TypeInfo(
        "java/lang/Long",
        true,
        LRETURN,
        insns =>
          insns.add(
            new MethodInsnNode(
              INVOKESTATIC,
              "scala/runtime/BoxesRunTime",
              "unboxToLong",
              "(Ljava/lang/Object;)J",
              false
            )
          ),
        insns =>
          insns.add(
            new MethodInsnNode(INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToLong", "(J)Ljava/lang/Long;", false)
          )
      )
    case "D" =>
      TypeInfo(
        "java/lang/Double",
        true,
        DRETURN,
        insns =>
          insns.add(
            new MethodInsnNode(
              INVOKESTATIC,
              "scala/runtime/BoxesRunTime",
              "unboxToDouble",
              "(Ljava/lang/Object;)D",
              false
            )
          ),
        insns =>
          insns.add(
            new MethodInsnNode(
              INVOKESTATIC,
              "scala/runtime/BoxesRunTime",
              "boxToDouble",
              "(D)Ljava/lang/Double;",
              false
            )
          )
      )
    case "Z" =>
      TypeInfo(
        "java/lang/Boolean",
        true,
        IRETURN,
        insns =>
          insns.add(
            new MethodInsnNode(
              INVOKESTATIC,
              "scala/runtime/BoxesRunTime",
              "unboxToBoolean",
              "(Ljava/lang/Object;)Z",
              false
            )
          ),
        insns =>
          insns.add(
            new MethodInsnNode(
              INVOKESTATIC,
              "scala/runtime/BoxesRunTime",
              "boxToBoolean",
              "(Z)Ljava/lang/Boolean;",
              false
            )
          )
      )
    case "F" =>
      TypeInfo(
        "java/lang/Float",
        true,
        FRETURN,
        insns =>
          insns.add(
            new MethodInsnNode(
              INVOKESTATIC,
              "scala/runtime/BoxesRunTime",
              "unboxToFloat",
              "(Ljava/lang/Object;)F",
              false
            )
          ),
        insns =>
          insns.add(
            new MethodInsnNode(INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToFloat", "(F)Ljava/lang/Float;", false)
          )
      )
    case desc if desc.startsWith("L") && desc.endsWith(";") =>
      val internalName = desc.substring(1, desc.length - 1)
      TypeInfo(
        internalName,
        false,
        ARETURN,
        insns => insns.add(new TypeInsnNode(CHECKCAST, internalName)),
        insns => () /* no boxing needed for reference types */
      )
    case desc =>
      // Fallback for Object or unknown — treat as reference type
      TypeInfo(
        "java/lang/Object",
        false,
        ARETURN,
        insns => (), /* no cast needed for Object */
        insns => () /* no boxing needed */
      )
  }

  // ============================================================================
  // Scala 3.2 Patching Strategy (Early VarHandle variant)
  // ============================================================================

  /** Patches Scala 3.2 lazy vals to 3.8+ format.
    *
    * Scala 3.2 bytecode is nearly identical to 3.0-3.1 — the only difference is in clinit
    * (getDeclaredField+getOffsetStatic vs getOffset), which the backward-walking removal code handles transparently.
    * Delegates to the 3.0-3.1 patching logic.
    */
  private def patchScala32x(
      bytes: Array[Byte],
      classInfo: sloth.classfile.ClassInfo,
      lazyVals: Seq[LazyValInfo],
      name: String,
      classLoader: Option[ClassLoader] = None
  ): PatchResult = {
    patchScala30x_31x(bytes, classInfo, lazyVals, name, classLoader = classLoader)
  }

  // ============================================================================
  // Scala 3.3-3.7 Patching Strategy (Unsafe → VarHandle transformation)
  // ============================================================================

  /** Patches companion pair where BOTH class and object have lazy vals.
    *
    * This is the complex case where we need to patch both files independently.
    */
  private def patchCompanionPairBothHaveLazyVals33x_37x(
      companionObjectName: String,
      className: String,
      companionObjectInfo: sloth.classfile.ClassInfo,
      classInfo: sloth.classfile.ClassInfo,
      companionObjectBytes: Array[Byte],
      classBytes: Array[Byte],
      objectLazyVals: Seq[LazyValInfo],
      classLazyVals: Seq[LazyValInfo],
      classLoader: Option[ClassLoader] = None
  ): PatchResult = {
    try {
      // Check if object lazy vals have OFFSET in companion class
      val objectHasCompanionOffset =
        objectLazyVals.exists(_.offsetFieldLocation == OffsetFieldLocation.InCompanionClass)

      if (objectHasCompanionOffset) {
        // Complex case: object has OFFSET in companion class, AND class has its own lazy vals
        // Step 1: Patch object (which also patches class to remove OFFSET fields)
        val objectPatchResult = patchScala33x_37x(
          companionObjectBytes,
          companionObjectInfo,
          objectLazyVals,
          companionObjectName,
          Some((className, classInfo, classBytes)),
          classLoader = classLoader
        )

        objectPatchResult match {
          case PatchResult.PatchedPair(_, _, patchedObjectBytes, intermediatePatchedClassBytes) =>
            // Step 2: Parse the intermediate patched class bytes and patch again for class's own lazy vals
            val intermediateClassInfo = ClassfileParser.parse(intermediatePatchedClassBytes).toOption.get
            val reader = new ClassReader(intermediatePatchedClassBytes)
            val classNode = new ClassNode(ASM9)
            reader.accept(classNode, ClassReader.EXPAND_FRAMES)

            // Patch the class node for its own lazy vals
            patchClassNode33x_37x(classNode, className, classLazyVals)

            // Write back to bytes
            val writer = makeClassWriter(classLoader)
            classNode.accept(writer)
            val finalPatchedClassBytes = writer.toByteArray

            PatchResult.PatchedPair(companionObjectName, className, patchedObjectBytes, finalPatchedClassBytes)

          case PatchResult.Failed(err) =>
            PatchResult.Failed(s"Failed to patch companion object: $err")
          case _ =>
            PatchResult.Failed("Unexpected result when patching companion object")
        }
      } else {
        // Simpler case: object has OFFSET in itself, class has its own lazy vals
        // Patch both independently
        val objectPatchResult = patchScala33x_37x(
          companionObjectBytes,
          companionObjectInfo,
          objectLazyVals,
          companionObjectName,
          None,
          None,
          classLoader = classLoader
        )
        val classPatchResult =
          patchScala33x_37x(classBytes, classInfo, classLazyVals, className, None, None, classLoader = classLoader)

        (objectPatchResult, classPatchResult) match {
          case (PatchResult.PatchedSingle(_, objBytes), PatchResult.PatchedSingle(_, clsBytes)) =>
            PatchResult.PatchedPair(companionObjectName, className, objBytes, clsBytes)
          case (PatchResult.Failed(err), _) =>
            PatchResult.Failed(s"Failed to patch companion object: $err")
          case (_, PatchResult.Failed(err)) =>
            PatchResult.Failed(s"Failed to patch companion class: $err")
          case _ =>
            PatchResult.Failed("Unexpected patching result for companion pair")
        }
      }
    } catch {
      case e: Exception =>
        e.printStackTrace()
        PatchResult.Failed(s"Patching failed for companion pair: ${e.getMessage}")
    }
  }

  /** Patches Scala 3.3-3.7 lazy vals to 3.8+ format.
    *
    * Transforms Unsafe-based lazy vals to VarHandle-based implementation. When companion class data is provided,
    * patches both the companion object and class to move OFFSET fields to VarHandle.
    *
    * @param companionInfo
    *   Optional tuple of (className, classInfo, classBytes) for companion class that contains OFFSET fields
    */
  private def patchScala33x_37x(
      bytes: Array[Byte],
      classInfo: sloth.classfile.ClassInfo,
      lazyVals: Seq[LazyValInfo],
      name: String,
      companionInfo: Option[(String, sloth.classfile.ClassInfo, Array[Byte])],
      unused: Option[Any] = None, // For compatibility with old signature
      classLoader: Option[ClassLoader] = None
  ): PatchResult = {
    try {
      companionInfo match {
        case Some((companionClassName, companionClassInfo, companionClassBytes)) =>
          // This is a companion object with OFFSET fields in the companion class
          // We need to:
          // 1. Build offsetToVarHandle map from companion class OFFSET fields
          // 2. Patch companion class: remove OFFSET fields (no VarHandle needed)
          // 3. Patch companion object: add VarHandle fields, transform lzyINIT methods, patch <clinit>

          val companionReader = new ClassReader(companionClassBytes)
          val companionClassNode = new ClassNode(ASM9)
          companionReader.accept(companionClassNode, ClassReader.EXPAND_FRAMES)

          // Build offsetToVarHandle map from companion class OFFSET fields
          val offsetToVarHandle = buildOffsetToVarHandleMap(companionClassNode, lazyVals)

          // Patch companion class: just remove OFFSET fields and <clinit>
          patchCompanionClass33x_37x(companionClassNode, lazyVals)

          val companionWriter = makeClassWriter(classLoader)
          companionClassNode.accept(companionWriter)
          val patchedClassBytes = companionWriter.toByteArray

          // Patch companion object: add VarHandle fields, patch <clinit>, transform lzyINIT methods
          val objectReader = new ClassReader(bytes)
          val objectNode = new ClassNode(ASM9)
          objectReader.accept(objectNode, ClassReader.EXPAND_FRAMES)

          // Add VarHandle fields to the object
          offsetToVarHandle.values.toSet.foreach { varHandleName =>
            val fieldNode = new FieldNode(
              ASM9,
              ACC_PRIVATE | ACC_STATIC | ACC_FINAL,
              varHandleName,
              "Ljava/lang/invoke/VarHandle;",
              null,
              null
            )
            objectNode.fields.add(fieldNode)
          }

          // Patch <clinit> in object to initialize VarHandle fields
          objectNode.methods.asScala.find(_.name == "<clinit>").foreach { clinit =>
            patchClinitForCompanionObject(clinit, classInfo.name, lazyVals, offsetToVarHandle)
          }

          // Patch lzyINIT methods in object to use VarHandle
          objectNode.methods.asScala.filter(_.name.matches(".*\\$lzyINIT\\d+")).foreach { lzyInit =>
            patchLzyInitMethod33x_37x(lzyInit, classInfo.name, offsetToVarHandle)
          }

          // Add MethodHandles$Lookup inner class reference to object
          val hasLookupInner = objectNode.innerClasses.asScala.exists { inner =>
            inner.name == "java/lang/invoke/MethodHandles$Lookup"
          }
          if (!hasLookupInner) {
            objectNode.innerClasses.add(
              new InnerClassNode(
                "java/lang/invoke/MethodHandles$Lookup",
                "java/lang/invoke/MethodHandles",
                "Lookup",
                ACC_PUBLIC | ACC_FINAL | ACC_STATIC
              )
            )
          }

          val objectWriter = makeClassWriter(classLoader)
          objectNode.accept(objectWriter)
          val patchedObjectBytes = objectWriter.toByteArray

          PatchResult.PatchedPair(name, companionClassName, patchedObjectBytes, patchedClassBytes)

        case None =>
          // Standalone class/object with OFFSET fields in the same class
          val reader = new ClassReader(bytes)
          val classNode = new ClassNode(ASM9)
          reader.accept(classNode, ClassReader.EXPAND_FRAMES)

          // Perform patching on the class node
          patchClassNode33x_37x(classNode, classInfo.name, lazyVals)

          // Write back to bytes
          val writer = makeClassWriter(classLoader)
          classNode.accept(writer)
          val patchedBytes = writer.toByteArray

          PatchResult.PatchedSingle(name, patchedBytes)
      }
    } catch {
      case e: Exception =>
        e.printStackTrace()
        PatchResult.Failed(s"Patching failed: ${e.getMessage}")
    }
  }

  /** Patches companion class to remove OFFSET fields belonging to the companion object.
    *
    * IMPORTANT: Only removes OFFSET$_m_N fields (companion object's lazy vals). Preserves OFFSET$N fields (class's own
    * lazy vals) for subsequent patching. Does NOT remove <clinit> since it may contain initialization for the class's
    * own OFFSET fields.
    */
  private def patchCompanionClass33x_37x(classNode: ClassNode, lazyVals: Seq[LazyValInfo]): Unit = {
    // Only remove OFFSET$_m_N fields (companion object's lazy vals)
    // Preserve OFFSET$N fields (class's own lazy vals)
    val fieldsToRemove = scala.collection.mutable.Set[FieldNode]()
    classNode.fields.asScala.foreach { field =>
      if (field.desc == "J" && field.name.matches("OFFSET\\$_m_\\d+")) {
        fieldsToRemove.add(field)
      }
    }
    fieldsToRemove.foreach(classNode.fields.remove)

    // Remove initialization of OFFSET$_m_N fields from <clinit>
    // Keep the rest of <clinit> intact for class's own OFFSET fields
    classNode.methods.asScala.find(_.name == "<clinit>").foreach { clinit =>
      val instructions = clinit.instructions
      val toRemove = scala.collection.mutable.ListBuffer[AbstractInsnNode]()

      var current = instructions.getFirst
      while (current != null) {
        current match {
          case fieldInsn: FieldInsnNode
              if fieldInsn.getOpcode == PUTSTATIC &&
                fieldInsn.desc == "J" &&
                fieldInsn.name.matches("OFFSET\\$_m_\\d+") =>
            // Found PUTSTATIC for OFFSET$_m_N field
            // Remove the entire initialization sequence: ALOAD/LDC -> INVOKEVIRTUAL -> PUTSTATIC
            var prev = current.getPrevious
            var removeCount = 0
            // Work backwards to find the start of this field initialization
            while (prev != null && removeCount < 10) { // Safety limit
              prev match {
                case _: FieldInsnNode if prev.getOpcode == GETSTATIC =>
                  // Found start of initialization sequence
                  var temp = prev
                  while (temp != null && temp != current.getNext) {
                    toRemove += temp
                    temp = temp.getNext
                  }
                  removeCount = 999 // Break outer loop
                  prev = null
                case _ =>
                  removeCount += 1
                  prev = prev.getPrevious
              }
            }
          case _ =>
        }
        current = current.getNext
      }

      toRemove.foreach(instructions.remove)
    }
  }

  /** Core patching logic for Scala 3.3-3.7 lazy vals. */
  private def patchClassNode33x_37x(classNode: ClassNode, className: String, lazyVals: Seq[LazyValInfo]): Unit = {
    // Map of OFFSET field names to VarHandle field names
    val offsetToVarHandle = scala.collection.mutable.Map[String, String]()

    // Step 1: Remove OFFSET fields and collect names for VarHandle fields
    val fieldsToRemove = scala.collection.mutable.Set[FieldNode]()
    classNode.fields.asScala.foreach { field =>
      // Match both OFFSET patterns:
      // - OFFSET$_m_N (objects/companions)
      // - OFFSET$N (standalone classes)
      if (field.desc == "J" && (field.name.matches("OFFSET\\$_m_\\d+") || field.name.matches("OFFSET\\$\\d+"))) {
        // Extract index and find corresponding lazy val
        val companionPattern = "OFFSET\\$_m_(\\d+)".r
        val standalonePattern = "OFFSET\\$(\\d+)".r

        field.name match {
          case companionPattern(idx) =>
            val offsetIndex = idx.toInt
            // IMPORTANT: OFFSET$_m_N maps to the Nth lazy val (0-indexed), NOT by lazy val index
            // All lazy vals in a class have the same index (typically 1), so we map by position
            if (offsetIndex < lazyVals.size) {
              val lv = lazyVals(offsetIndex)
              val varHandleName = s"${lv.name}$$lzy${lv.index}$$lzyHandle"
              offsetToVarHandle(field.name) = varHandleName
            }
          case standalonePattern(idx) =>
            val offsetIndex = idx.toInt
            // IMPORTANT: OFFSET$N maps to the Nth lazy val (0-indexed), NOT by lazy val index
            if (offsetIndex < lazyVals.size) {
              val lv = lazyVals(offsetIndex)
              val varHandleName = s"${lv.name}$$lzy${lv.index}$$lzyHandle"
              offsetToVarHandle(field.name) = varHandleName
            }
          case _ =>
        }
        fieldsToRemove.add(field)
      }
    }
    fieldsToRemove.foreach(classNode.fields.remove)

    // Step 2: Add VarHandle fields
    // IMPORTANT: Even if no OFFSET fields were found (e.g., they were already removed by companion patching),
    // we still need to add VarHandle fields for all lazy vals in the lazyVals list
    val varHandlesToAdd = lazyVals.map { lv =>
      s"${lv.name}$$lzy${lv.index}$$lzyHandle"
    }.toSet

    // Check which VarHandle fields already exist
    val existingVarHandles = classNode.fields.asScala
      .filter(f => f.desc == "Ljava/lang/invoke/VarHandle;")
      .map(_.name)
      .toSet

    // Only add VarHandles that don't already exist
    (varHandlesToAdd -- existingVarHandles).foreach { varHandleName =>
      val fieldNode = new FieldNode(
        ASM9,
        ACC_PRIVATE | ACC_STATIC | ACC_FINAL,
        varHandleName,
        "Ljava/lang/invoke/VarHandle;",
        null,
        null
      )
      classNode.fields.add(fieldNode)
    }

    // Step 3: Patch <clinit> method
    // Build complete mapping: for VarHandles without OFFSET fields, we need to add initialization
    val allVarHandles = lazyVals.map { lv =>
      val varHandleName = s"${lv.name}$$lzy${lv.index}$$lzyHandle"
      val storageFieldName = s"${lv.name}$$lzy${lv.index}"
      (varHandleName, storageFieldName)
    }.toMap

    // Step 3b: Patch or create <clinit> if needed
    val clinitOpt = classNode.methods.asScala.find(_.name == "<clinit>")
    clinitOpt match {
      case Some(clinit) =>
        // Patch existing <clinit>
        patchClinitMethod33x_37x(clinit, className, offsetToVarHandle.toMap, allVarHandles)

      case None if allVarHandles.nonEmpty =>
        // Create new <clinit> to initialize VarHandle fields
        val clinit = new MethodNode(
          ASM9,
          ACC_STATIC,
          "<clinit>",
          "()V",
          null,
          null
        )

        // Add VarHandle initialization for each lazy val
        allVarHandles.foreach { case (varHandleName, storageFieldName) =>
          // MethodHandles.lookup()
          clinit.instructions.add(
            new MethodInsnNode(
              INVOKESTATIC,
              "java/lang/invoke/MethodHandles",
              "lookup",
              "()Ljava/lang/invoke/MethodHandles$Lookup;",
              false
            )
          )

          // LDC <Class>
          clinit.instructions.add(new LdcInsnNode(org.objectweb.asm.Type.getObjectType(className.replace('.', '/'))))

          // LDC <FieldName>
          clinit.instructions.add(new LdcInsnNode(storageFieldName))

          // LDC Object.class
          clinit.instructions.add(new LdcInsnNode(org.objectweb.asm.Type.getType("Ljava/lang/Object;")))

          // INVOKEVIRTUAL Lookup.findVarHandle
          clinit.instructions.add(
            new MethodInsnNode(
              INVOKEVIRTUAL,
              "java/lang/invoke/MethodHandles$Lookup",
              "findVarHandle",
              "(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;)Ljava/lang/invoke/VarHandle;",
              false
            )
          )

          // PUTSTATIC <Class>.<varHandleName>
          clinit.instructions.add(
            new FieldInsnNode(
              PUTSTATIC,
              className.replace('.', '/'),
              varHandleName,
              "Ljava/lang/invoke/VarHandle;"
            )
          )
        }

        // RETURN
        clinit.instructions.add(new InsnNode(RETURN))

        // Add method to class
        classNode.methods.add(clinit)

      case None =>
      // No clinit and no VarHandles to initialize, do nothing
    }

    // Step 4: Patch lzyINIT methods
    classNode.methods.asScala.filter(_.name.matches(".*\\$lzyINIT\\d+")).foreach { lzyInit =>
      patchLzyInitMethod33x_37x(lzyInit, className, offsetToVarHandle.toMap)
    }

    // Step 5: Add MethodHandles$Lookup inner class reference
    val hasLookupInner = classNode.innerClasses.asScala.exists { inner =>
      inner.name == "java/lang/invoke/MethodHandles$Lookup"
    }
    if (!hasLookupInner) {
      classNode.innerClasses.add(
        new InnerClassNode(
          "java/lang/invoke/MethodHandles$Lookup",
          "java/lang/invoke/MethodHandles",
          "Lookup",
          ACC_PUBLIC | ACC_FINAL | ACC_STATIC
        )
      )
    }
  }

  /** Patches <clinit> for 3.3-3.7: Unsafe offset initialization → VarHandle lookup */
  private def patchClinitMethod33x_37x(
      method: MethodNode,
      className: String,
      offsetToVarHandle: Map[String, String],
      allVarHandles: Map[String, String] // varHandleName -> storageFieldName
  ): Unit = {
    val instructions = method.instructions
    val toRemove = scala.collection.mutable.ArrayBuffer[AbstractInsnNode]()
    val toInsert = scala.collection.mutable.ArrayBuffer[(AbstractInsnNode, java.util.List[AbstractInsnNode])]()

    // Find pattern:
    // GETSTATIC LazyVals$.MODULE$
    // LDC <Class>
    // LDC <FieldName>
    // INVOKEVIRTUAL Class.getDeclaredField
    // INVOKEVIRTUAL LazyVals$.getOffsetStatic
    // PUTSTATIC <Class>.OFFSET$_m_N

    var current = instructions.getFirst
    while (current != null) {
      current match {
        case getStatic: FieldInsnNode
            if getStatic.getOpcode == GETSTATIC &&
              getStatic.owner == LazyValsObj &&
              getStatic.name == "MODULE$" =>
          // Found start of pattern
          var next1 = getStatic.getNext
          var next2 = if (next1 != null) next1.getNext else null
          var next3 = if (next2 != null) next2.getNext else null
          var next4 = if (next3 != null) next3.getNext else null
          var next5 = if (next4 != null) next4.getNext else null

          (next1, next2, next3, next4, next5) match {
            case (
                  ldcClass: LdcInsnNode,
                  ldcField: LdcInsnNode,
                  getDeclaredField: MethodInsnNode,
                  getOffsetStatic: MethodInsnNode,
                  putStatic: FieldInsnNode
                )
                if ldcClass.cst.isInstanceOf[org.objectweb.asm.Type] &&
                  ldcField.cst.isInstanceOf[String] &&
                  getDeclaredField.getOpcode == INVOKEVIRTUAL &&
                  getDeclaredField.owner == "java/lang/Class" &&
                  getDeclaredField.name == "getDeclaredField" &&
                  getOffsetStatic.getOpcode == INVOKEVIRTUAL &&
                  getOffsetStatic.owner == LazyValsObj &&
                  getOffsetStatic.name == "getOffsetStatic" &&
                  putStatic.getOpcode == PUTSTATIC =>
              // Pattern matched!
              val classType = ldcClass.cst.asInstanceOf[org.objectweb.asm.Type]
              val fieldName = ldcField.cst.asInstanceOf[String]
              val offsetFieldName = putStatic.name

              offsetToVarHandle.get(offsetFieldName).foreach { varHandleName =>
                // Mark for removal
                toRemove.addAll(Seq(getStatic, next1, next2, next3, next4, next5))

                // Create replacement instructions
                val replacement = new java.util.ArrayList[AbstractInsnNode]()

                // INVOKESTATIC MethodHandles.lookup
                replacement.add(
                  new MethodInsnNode(
                    INVOKESTATIC,
                    "java/lang/invoke/MethodHandles",
                    "lookup",
                    "()Ljava/lang/invoke/MethodHandles$Lookup;",
                    false
                  )
                )

                // LDC <Class>
                replacement.add(new LdcInsnNode(classType))

                // LDC <FieldName>
                replacement.add(new LdcInsnNode(fieldName))

                // LDC Object.class
                replacement.add(new LdcInsnNode(org.objectweb.asm.Type.getType("Ljava/lang/Object;")))

                // INVOKEVIRTUAL Lookup.findVarHandle
                replacement.add(
                  new MethodInsnNode(
                    INVOKEVIRTUAL,
                    "java/lang/invoke/MethodHandles$Lookup",
                    "findVarHandle",
                    "(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;)Ljava/lang/invoke/VarHandle;",
                    false
                  )
                )

                // PUTSTATIC <Class>.<varHandleName>
                replacement.add(
                  new FieldInsnNode(
                    PUTSTATIC,
                    classType.getInternalName,
                    varHandleName,
                    "Ljava/lang/invoke/VarHandle;"
                  )
                )

                toInsert.append((getStatic, replacement))
              }

            case _ =>
            // Pattern didn't match completely, continue
          }

        case _ =>
        // Not the start of our pattern
      }
      current = current.getNext
    }

    // Apply transformations
    toInsert.foreach { case (anchor, newInsns) =>
      newInsns.asScala.foreach(instructions.insertBefore(anchor, _))
    }
    toRemove.foreach(instructions.remove)

    // Add initialization for VarHandles that don't have OFFSET fields (e.g., after companion patching)
    val initializedVarHandles = offsetToVarHandle.values.toSet
    val uninitializedVarHandles = allVarHandles.filterKeys(!initializedVarHandles.contains(_))

    if (uninitializedVarHandles.nonEmpty) {
      // Find a good insertion point (before RETURN)
      var returnInsn: AbstractInsnNode = null
      var current = instructions.getLast
      while (current != null && returnInsn == null) {
        if (current.getOpcode == RETURN) {
          returnInsn = current
        }
        current = current.getPrevious
      }

      if (returnInsn != null) {
        uninitializedVarHandles.foreach { case (varHandleName, storageFieldName) =>
          val newInsns = new java.util.ArrayList[AbstractInsnNode]()

          // INVOKESTATIC MethodHandles.lookup
          newInsns.add(
            new MethodInsnNode(
              INVOKESTATIC,
              "java/lang/invoke/MethodHandles",
              "lookup",
              "()Ljava/lang/invoke/MethodHandles$Lookup;",
              false
            )
          )

          // LDC <Class>
          newInsns.add(new LdcInsnNode(org.objectweb.asm.Type.getObjectType(className.replace('.', '/'))))

          // LDC <FieldName>
          newInsns.add(new LdcInsnNode(storageFieldName))

          // LDC Object.class
          newInsns.add(new LdcInsnNode(org.objectweb.asm.Type.getType("Ljava/lang/Object;")))

          // INVOKEVIRTUAL Lookup.findVarHandle
          newInsns.add(
            new MethodInsnNode(
              INVOKEVIRTUAL,
              "java/lang/invoke/MethodHandles$Lookup",
              "findVarHandle",
              "(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;)Ljava/lang/invoke/VarHandle;",
              false
            )
          )

          // PUTSTATIC <Class>.<varHandleName>
          newInsns.add(
            new FieldInsnNode(
              PUTSTATIC,
              className.replace('.', '/'),
              varHandleName,
              "Ljava/lang/invoke/VarHandle;"
            )
          )

          newInsns.asScala.foreach(instructions.insertBefore(returnInsn, _))
        }
      }
    }
  }

  /** Patches lzyINIT for 3.3-3.7: objCAS → VarHandle.compareAndSet */
  private def patchLzyInitMethod33x_37x(
      method: MethodNode,
      className: String,
      offsetToVarHandle: Map[String, String]
  ): Unit = {
    val instructions = method.instructions
    val toRemove = scala.collection.mutable.ArrayBuffer[AbstractInsnNode]()
    val toInsert = scala.collection.mutable.ArrayBuffer[(AbstractInsnNode, java.util.List[AbstractInsnNode])]()

    // Find pattern:
    // GETSTATIC LazyVals$.MODULE$
    // ALOAD 0
    // GETSTATIC <Class>.OFFSET$_m_N
    // [expected value]
    // [new value]
    // INVOKEVIRTUAL LazyVals$.objCAS

    var current = instructions.getFirst
    while (current != null) {
      current match {
        case getStatic: FieldInsnNode
            if getStatic.getOpcode == GETSTATIC &&
              getStatic.owner == LazyValsObj &&
              getStatic.name == "MODULE$" =>
          // Found potential start of CAS pattern
          var next1 = getStatic.getNext
          var next2 = if (next1 != null) next1.getNext else null

          (next1, next2) match {
            case (aload: VarInsnNode, getOffset: FieldInsnNode)
                if aload.getOpcode == ALOAD &&
                  aload.`var` == 0 &&
                  getOffset.getOpcode == GETSTATIC &&
                  (getOffset.name.matches("OFFSET\\$_m_\\d+") || getOffset.name.matches("OFFSET\\$\\d+")) =>
              // Pattern start matched, find objCAS call
              var scanPtr = next2.getNext
              var expectedInsns = scala.collection.mutable.ArrayBuffer[AbstractInsnNode]()
              var foundObjCas = false
              var objCasNode: MethodInsnNode = null

              // Collect instructions until we hit objCAS (max 10 instructions)
              var count = 0
              while (scanPtr != null && count < 20 && !foundObjCas) {
                scanPtr match {
                  case methodInsn: MethodInsnNode
                      if methodInsn.getOpcode == INVOKEVIRTUAL &&
                        methodInsn.owner == LazyValsObj &&
                        methodInsn.name == "objCAS" =>
                    foundObjCas = true
                    objCasNode = methodInsn

                  case _ =>
                    expectedInsns.append(scanPtr)
                }
                scanPtr = scanPtr.getNext
                count += 1
              }

              if (foundObjCas && objCasNode != null) {
                offsetToVarHandle.get(getOffset.name).foreach { varHandleName =>
                  // Mark for removal: GETSTATIC LazyVals$, ALOAD 0, GETSTATIC OFFSET, objCAS
                  toRemove.addAll(Seq(getStatic, next1, next2, objCasNode))

                  // Create replacement
                  val replacement = new java.util.ArrayList[AbstractInsnNode]()

                  // GETSTATIC <Class>.<varHandleName>
                  replacement.add(
                    new FieldInsnNode(
                      GETSTATIC,
                      className.replace('.', '/'),
                      varHandleName,
                      "Ljava/lang/invoke/VarHandle;"
                    )
                  )

                  // ALOAD 0
                  replacement.add(new VarInsnNode(ALOAD, 0))

                  // [expected and new values are already on the stack, keep them]

                  // Insert replacement before the pattern
                  toInsert.append((getStatic, replacement))

                  // Add VarHandle.compareAndSet after the arguments
                  val casCall = new java.util.ArrayList[AbstractInsnNode]()
                  casCall.add(
                    new MethodInsnNode(
                      INVOKEVIRTUAL,
                      "java/lang/invoke/VarHandle",
                      "compareAndSet",
                      "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Z",
                      false
                    )
                  )
                  toInsert.append((objCasNode, casCall))
                }
              }

            case _ =>
            // Pattern didn't match
          }

        case _ =>
        // Not the start of our pattern
      }
      current = current.getNext
    }

    // Apply transformations
    toInsert.foreach { case (anchor, newInsns) =>
      newInsns.asScala.foreach(instructions.insertBefore(anchor, _))
    }
    toRemove.foreach(instructions.remove)
  }

  /** Builds offsetToVarHandle map by scanning companion class for OFFSET fields. */
  private def buildOffsetToVarHandleMap(
      companionClassNode: ClassNode,
      lazyVals: Seq[LazyValInfo]
  ): Map[String, String] = {
    val offsetToVarHandle = scala.collection.mutable.Map[String, String]()

    companionClassNode.fields.asScala.foreach { field =>
      if (field.desc == "J" && (field.name.matches("OFFSET\\$_m_\\d+") || field.name.matches("OFFSET\\$\\d+"))) {
        val companionPattern = "OFFSET\\$_m_(\\d+)".r
        val standalonePattern = "OFFSET\\$(\\d+)".r

        field.name match {
          case companionPattern(idx) =>
            val offsetIndex = idx.toInt
            // IMPORTANT: OFFSET$_m_N maps to the Nth lazy val (0-indexed), NOT by lazy val index
            if (offsetIndex < lazyVals.size) {
              val lv = lazyVals(offsetIndex)
              val varHandleName = s"${lv.name}$$lzy${lv.index}$$lzyHandle"
              offsetToVarHandle(field.name) = varHandleName
            }
          case standalonePattern(idx) =>
            val offsetIndex = idx.toInt
            // IMPORTANT: OFFSET$N maps to the Nth lazy val (0-indexed), NOT by lazy val index
            if (offsetIndex < lazyVals.size) {
              val lv = lazyVals(offsetIndex)
              val varHandleName = s"${lv.name}$$lzy${lv.index}$$lzyHandle"
              offsetToVarHandle(field.name) = varHandleName
            }
          case _ =>
        }
      }
    }

    offsetToVarHandle.toMap
  }

  /** Patches companion class: removes OFFSET fields, adds VarHandle fields, patches/creates <clinit>. */
  private def patchCompanionClassWithVarHandles(
      companionClassNode: ClassNode,
      companionClassName: String,
      lazyVals: Seq[LazyValInfo],
      offsetToVarHandle: Map[String, String]
  ): Unit = {
    // Step 1: Remove OFFSET fields
    val fieldsToRemove = scala.collection.mutable.Set[FieldNode]()
    companionClassNode.fields.asScala.foreach { field =>
      if (field.desc == "J" && (field.name.matches("OFFSET\\$_m_\\d+") || field.name.matches("OFFSET\\$\\d+"))) {
        fieldsToRemove.add(field)
      }
    }
    fieldsToRemove.foreach(companionClassNode.fields.remove)

    // Step 2: Add VarHandle fields
    offsetToVarHandle.values.toSet.foreach { varHandleName =>
      val fieldNode = new FieldNode(
        ASM9,
        ACC_PRIVATE | ACC_STATIC | ACC_FINAL,
        varHandleName,
        "Ljava/lang/invoke/VarHandle;",
        null,
        null
      )
      companionClassNode.fields.add(fieldNode)
    }

    // Step 3: Patch or create <clinit> method
    // Build complete mapping for all VarHandles
    val allVarHandles = lazyVals.map { lv =>
      val varHandleName = s"${lv.name}$$lzy${lv.index}$$lzyHandle"
      val storageFieldName = s"${lv.name}$$lzy${lv.index}"
      (varHandleName, storageFieldName)
    }.toMap

    val clinitOpt = companionClassNode.methods.asScala.find(_.name == "<clinit>")
    clinitOpt match {
      case Some(clinit) =>
        // Patch existing <clinit>: replace OFFSET initialization with VarHandle initialization
        patchClinitMethod33x_37x(clinit, companionClassName, offsetToVarHandle, allVarHandles)
      case None =>
        // Create new <clinit> to initialize VarHandle fields
        val clinit = new MethodNode(
          ASM9,
          ACC_STATIC,
          "<clinit>",
          "()V",
          null,
          null
        )
        val instructions = clinit.instructions

        // Generate VarHandle initialization for each lazy val
        offsetToVarHandle.foreach { case (offsetField, varHandleName) =>
          // Find the corresponding lazy val
          lazyVals.find(lv => varHandleName == s"${lv.name}$$lzy${lv.index}$$lzyHandle").foreach { lv =>
            // MethodHandles.lookup()
            instructions.add(
              new MethodInsnNode(
                INVOKESTATIC,
                "java/lang/invoke/MethodHandles",
                "lookup",
                "()Ljava/lang/invoke/MethodHandles$Lookup;",
                false
              )
            )

            // LDC class
            instructions.add(new LdcInsnNode(Type.getObjectType(companionClassName.replace('.', '/'))))

            // LDC field name
            instructions.add(new LdcInsnNode(lv.storageField.name))

            // LDC field type
            instructions.add(new LdcInsnNode(Type.getType(lv.storageField.descriptor)))

            // findVarHandle
            instructions.add(
              new MethodInsnNode(
                INVOKEVIRTUAL,
                "java/lang/invoke/MethodHandles$Lookup",
                "findVarHandle",
                "(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;)Ljava/lang/invoke/VarHandle;",
                false
              )
            )

            // PUTSTATIC
            instructions.add(
              new FieldInsnNode(
                PUTSTATIC,
                companionClassName.replace('.', '/'),
                varHandleName,
                "Ljava/lang/invoke/VarHandle;"
              )
            )
          }
        }

        // RETURN
        instructions.add(new InsnNode(RETURN))

        companionClassNode.methods.add(clinit)
    }

    // Step 4: Add MethodHandles$Lookup inner class reference
    val hasLookupInner = companionClassNode.innerClasses.asScala.exists { inner =>
      inner.name == "java/lang/invoke/MethodHandles$Lookup"
    }
    if (!hasLookupInner) {
      companionClassNode.innerClasses.add(
        new InnerClassNode(
          "java/lang/invoke/MethodHandles$Lookup",
          "java/lang/invoke/MethodHandles",
          "Lookup",
          ACC_PUBLIC | ACC_FINAL | ACC_STATIC
        )
      )
    }
  }

  /** Patches <clinit> in companion object to initialize VarHandle fields at the beginning. */
  private def patchClinitForCompanionObject(
      method: MethodNode,
      objectClassName: String,
      lazyVals: Seq[LazyValInfo],
      offsetToVarHandle: Map[String, String]
  ): Unit = {
    val instructions = method.instructions
    val firstInsn = instructions.getFirst

    // Generate VarHandle initialization for each lazy val and insert at the beginning
    offsetToVarHandle.foreach { case (offsetField, varHandleName) =>
      lazyVals.find(lv => varHandleName == s"${lv.name}$$lzy${lv.index}$$lzyHandle").foreach { lv =>
        val newInsns = new java.util.ArrayList[AbstractInsnNode]()

        // MethodHandles.lookup()
        newInsns.add(
          new MethodInsnNode(
            INVOKESTATIC,
            "java/lang/invoke/MethodHandles",
            "lookup",
            "()Ljava/lang/invoke/MethodHandles$Lookup;",
            false
          )
        )

        // LDC class (the object class where storage field is)
        newInsns.add(new LdcInsnNode(Type.getObjectType(objectClassName.replace('.', '/'))))

        // LDC field name
        newInsns.add(new LdcInsnNode(lv.storageField.name))

        // LDC field type
        newInsns.add(new LdcInsnNode(Type.getType(lv.storageField.descriptor)))

        // findVarHandle
        newInsns.add(
          new MethodInsnNode(
            INVOKEVIRTUAL,
            "java/lang/invoke/MethodHandles$Lookup",
            "findVarHandle",
            "(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/Class;)Ljava/lang/invoke/VarHandle;",
            false
          )
        )

        // PUTSTATIC (store in object class)
        newInsns.add(
          new FieldInsnNode(
            PUTSTATIC,
            objectClassName.replace('.', '/'),
            varHandleName,
            "Ljava/lang/invoke/VarHandle;"
          )
        )

        // Insert all instructions before the first instruction
        newInsns.asScala.foreach(instructions.insertBefore(firstInsn, _))
      }
    }
  }

  /** Patches lzyINIT method to use VarHandle from companion class instead of OFFSET. */
  private def patchLzyInitMethodWithCompanionVarHandle(
      method: MethodNode,
      companionClassName: String,
      offsetToVarHandle: Map[String, String]
  ): Unit = {
    val instructions = method.instructions
    val toRemove = scala.collection.mutable.Set[AbstractInsnNode]()
    val toInsert = scala.collection.mutable.ArrayBuffer[(AbstractInsnNode, java.util.List[AbstractInsnNode])]()

    // Find pattern:
    // GETSTATIC LazyVals$.MODULE$
    // ALOAD 0
    // GETSTATIC <CompanionClass>.OFFSET$_m_N
    // [expected value]
    // [new value]
    // INVOKEVIRTUAL LazyVals$.objCAS

    var current = instructions.getFirst
    while (current != null) {
      current match {
        case getStatic: FieldInsnNode
            if getStatic.getOpcode == GETSTATIC &&
              getStatic.owner == LazyValsObj &&
              getStatic.name == "MODULE$" =>
          var next1 = getStatic.getNext
          var next2 = if (next1 != null) next1.getNext else null

          (next1, next2) match {
            case (aload: VarInsnNode, getOffset: FieldInsnNode)
                if aload.getOpcode == ALOAD &&
                  aload.`var` == 0 &&
                  getOffset.getOpcode == GETSTATIC &&
                  (getOffset.name.matches("OFFSET\\$_m_\\d+") || getOffset.name.matches("OFFSET\\$\\d+")) =>
              // Pattern start matched, find objCAS call
              var scanPtr = next2.getNext
              var foundObjCas = false
              var objCasNode: MethodInsnNode = null

              var count = 0
              while (scanPtr != null && count < 20 && !foundObjCas) {
                scanPtr match {
                  case methodInsn: MethodInsnNode
                      if methodInsn.getOpcode == INVOKEVIRTUAL &&
                        methodInsn.owner == LazyValsObj &&
                        methodInsn.name == "objCAS" =>
                    foundObjCas = true
                    objCasNode = methodInsn

                  case _ =>
                }
                scanPtr = scanPtr.getNext
                count += 1
              }

              if (foundObjCas && objCasNode != null) {
                offsetToVarHandle.get(getOffset.name).foreach { varHandleName =>
                  // Mark for removal: GETSTATIC LazyVals$, ALOAD 0, GETSTATIC OFFSET, objCAS
                  toRemove.addAll(Seq(getStatic, next1, next2, objCasNode))

                  // Create replacement
                  val replacement = new java.util.ArrayList[AbstractInsnNode]()

                  // GETSTATIC <CompanionClass>.<varHandleName>
                  replacement.add(
                    new FieldInsnNode(
                      GETSTATIC,
                      companionClassName.replace('.', '/'),
                      varHandleName,
                      "Ljava/lang/invoke/VarHandle;"
                    )
                  )

                  // ALOAD 0
                  replacement.add(new VarInsnNode(ALOAD, 0))

                  // Insert replacement before the pattern
                  toInsert.append((getStatic, replacement))

                  // Add VarHandle.compareAndSet after the arguments
                  val casCall = new java.util.ArrayList[AbstractInsnNode]()
                  casCall.add(
                    new MethodInsnNode(
                      INVOKEVIRTUAL,
                      "java/lang/invoke/VarHandle",
                      "compareAndSet",
                      "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Z",
                      false
                    )
                  )
                  toInsert.append((objCasNode, casCall))
                }
              }

            case _ =>
          }

        case _ =>
      }
      current = current.getNext
    }

    // Apply transformations
    toInsert.foreach { case (anchor, newInsns) =>
      newInsns.asScala.foreach(instructions.insertBefore(anchor, _))
    }
    toRemove.foreach(instructions.remove)
  }
}
