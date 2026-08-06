package sloth.lazyval

import sloth.classfile.{ClassInfo, FieldInfo, MethodInfo}
import org.objectweb.asm.Opcodes
import scala.util.matching.Regex

/** Detects and classifies lazy vals in Scala 3 classfiles.
  *
  * Identifies lazy val implementation patterns from Scala 3.0 through 3.8+ and classifies them by version family.
  */
final class LazyValDetector:
  import scribe.{info, debug, warn, error}
  import LazyValDetector._

  /** Detects lazy vals in a classfile and classifies their implementation version.
    *
    * @param classInfo
    *   The parsed classfile (typically the companion object for lazy vals)
    * @param companionClassInfo
    *   Optional companion class (e.g., Foo for Foo$ companion object). Checked for OFFSET fields that may be split
    *   across companion class.
    * @return
    *   Detection result indicating if lazy vals were found and their versions
    */
  def detect(classInfo: ClassInfo, companionClassInfo: Option[ClassInfo] = None): LazyValDetectionResult =
    debug(s"Detecting lazy vals in class ${classInfo.name}")
    companionClassInfo.foreach(c => debug(s"  with companion class ${c.name}"))

    // Step 1: Find all potential lazy val fields
    val storageFields = findStorageFields(classInfo.fields)

    if storageFields.isEmpty then
      debug("No lazy val storage fields found")
      return LazyValDetectionResult.NoLazyVals

    info(s"Found ${storageFields.size} potential lazy val storage fields")

    // Step 2: Parse <clinit> to build offset->storageField mapping
    // Check both current class and companion class for OFFSET field initialization
    val offsetMapping = buildOffsetMapping(classInfo, companionClassInfo)
    debug(s"Built offset mapping: $offsetMapping")

    // Step 3: Collect fields from both current class and companion
    val allFields = classInfo.fields ++ companionClassInfo.map(_.fields).getOrElse(Seq.empty)

    // Step 4: Build LazyValInfo for each detected lazy val
    val lazyVals = storageFields.flatMap { storageField =>
      buildLazyValInfo(storageField, classInfo, companionClassInfo, offsetMapping, allFields)
    }

    if lazyVals.isEmpty then
      debug("Could not build LazyValInfo for any storage fields")
      return LazyValDetectionResult.NoLazyVals

    info(s"Successfully identified ${lazyVals.size} lazy vals")

    // Step 4: Determine overall version
    val versions = lazyVals.map(_.version).distinct

    if versions.size == 1 then LazyValDetectionResult.LazyValsFound(lazyVals, versions.head)
    else
      error(s"Mixed versions detected: $versions")
      LazyValDetectionResult.MixedVersions(lazyVals)

  /** Builds a mapping from OFFSET field names to lazy val storage field names.
    *
    * Parses <clinit> bytecode to find LDC instructions that load field names before PUTSTATIC OFFSET$_m_<N>
    * instructions. Checks both the current class and optional companion class.
    *
    * Returns Map[offsetFieldName -> storageFieldName]
    */
  private def buildOffsetMapping(
      classInfo: ClassInfo,
      companionClassInfo: Option[ClassInfo]
  ): Map[String, String] =
    val mapping = scala.collection.mutable.Map[String, String]()

    // Helper to parse a single <clinit> method
    def parseClinitMethod(method: MethodInfo, sourceClass: String): Unit =
      val bytecode = method.bytecodeText
      val lines = bytecode.split("\n")
      var lastLdcField: Option[String] = None

      lines.foreach { line =>
        val trimmed = line.trim

        // Check for LDC with a field name containing "$lzy"
        if trimmed.startsWith("LDC \"") && trimmed.contains("$lzy") then
          val fieldNamePattern = """LDC "([^"]+\$lzy\d+)"""".r
          fieldNamePattern.findFirstMatchIn(trimmed).foreach { m =>
            lastLdcField = Some(m.group(1))
            debug(s"Found LDC for field in $sourceClass: ${m.group(1)}")
          }

        // Check for LDC with a bitmap field name
        else if trimmed.startsWith("LDC \"") && trimmed.contains("bitmap$") then
          val bitmapPattern = """LDC "(\d+bitmap\$\d+)"""".r
          bitmapPattern.findFirstMatchIn(trimmed).foreach { m =>
            lastLdcField = Some(m.group(1))
            debug(s"Found LDC for bitmap field in $sourceClass: ${m.group(1)}")
          }

        // Check for PUTSTATIC OFFSET$_m_<N> (companion object lazy vals)
        else if trimmed.contains("PUTSTATIC") && trimmed.contains("OFFSET$_m_") then
          val offsetPattern = """OFFSET\$_m_(\d+)""".r
          offsetPattern.findFirstMatchIn(trimmed).foreach { m =>
            val offsetField = s"OFFSET$$_m_${m.group(1)}"
            lastLdcField.foreach { storageField =>
              mapping(offsetField) = storageField
              debug(s"Mapped $offsetField -> $storageField (from $sourceClass)")
            }
            lastLdcField = None
          }

        // Check for PUTSTATIC OFFSET$<N> (instance lazy vals in classes)
        else if trimmed.contains("PUTSTATIC") && trimmed.contains("OFFSET$") then
          val offsetPattern = """OFFSET\$(\d+)""".r
          offsetPattern.findFirstMatchIn(trimmed).foreach { m =>
            val offsetField = s"OFFSET$$${m.group(1)}"
            lastLdcField.foreach { storageField =>
              mapping(offsetField) = storageField
              debug(s"Mapped $offsetField -> $storageField (from $sourceClass)")
            }
            lastLdcField = None
          }
      }

    // Parse current class <clinit>
    classInfo.methods.find(_.name == "<clinit>") match
      case Some(method) =>
        debug(s"Parsing <clinit> from ${classInfo.name}")
        parseClinitMethod(method, classInfo.name)
      case None => debug(s"No <clinit> in ${classInfo.name}")

    // Parse companion class <clinit> if present
    companionClassInfo.foreach { companionInfo =>
      companionInfo.methods.find(_.name == "<clinit>") match
        case Some(method) =>
          debug(s"Parsing <clinit> from companion class ${companionInfo.name}")
          parseClinitMethod(method, companionInfo.name)
        case None => debug(s"No <clinit> in ${companionInfo.name}")
    }

    mapping.toMap

  /** Finds all lazy val storage fields in the class.
    *
    * Storage fields follow the pattern: <name>$lzy<N>
    */
  private def findStorageFields(fields: Seq[FieldInfo]): Seq[FieldInfo] =
    fields.filter { field =>
      storageFieldPattern.matches(field.name) &&
      !field.name.endsWith("$lzyHandle") // Exclude VarHandle fields
    }

  /** Builds complete LazyValInfo for a storage field. */
  private def buildLazyValInfo(
      storageField: FieldInfo,
      classInfo: ClassInfo,
      companionClassInfo: Option[ClassInfo],
      offsetMapping: Map[String, String],
      allFields: Seq[FieldInfo]
  ): Option[LazyValInfo] =
    val nameAndIndex = parseStorageFieldName(storageField.name)

    nameAndIndex.flatMap { case (name, index) =>
      // Find associated fields using the offset mapping
      // Search in current class first, then companion class
      val (offsetField, offsetLocation) = findOffsetFieldForStorage(
        storageField.name,
        offsetMapping,
        classInfo.fields,
        companionClassInfo.map(_.fields).getOrElse(Seq.empty)
      )
      val bitmapField = findBitmapFieldForStorage(storageField.name, offsetMapping, allFields)
      val varHandleField = findVarHandleField(name, index, classInfo.fields)

      // Find associated methods
      val initMethod = findInitMethod(name, index, classInfo.methods)
      val accessorMethod = findAccessorMethod(name, classInfo.methods)

      // Determine version based on field and method patterns
      val version = determineVersion(
        storageField,
        offsetField,
        offsetLocation,
        bitmapField,
        varHandleField,
        initMethod,
        accessorMethod,
        classInfo,
        companionClassInfo
      )

      version match
        case ScalaVersion.Unknown(_)
            if offsetField.isEmpty && bitmapField.isEmpty &&
              varHandleField.isEmpty && initMethod.isEmpty && !isVolatile(storageField.access) =>
          debug(
            s"Skipping field ${storageField.name}: matches $$lzy pattern but has no lazy val infrastructure (likely eager companion object reference)"
          )
          None
        case _ =>
          Some(
            LazyValInfo(
              name = name,
              index = index,
              offsetField = offsetField,
              offsetFieldLocation = offsetLocation,
              bitmapField = bitmapField,
              storageField = storageField,
              varHandleField = varHandleField,
              initMethod = initMethod,
              accessorMethod = accessorMethod,
              version = version
            )
          )
    }

  /** Parses storage field name to extract lazy val name and index.
    *
    * Example: "simpleLazy$lzy1" -> ("simpleLazy", 1)
    */
  private def parseStorageFieldName(fieldName: String): Option[(String, Int)] =
    storageFieldPattern.findFirstMatchIn(fieldName).map { m =>
      val name = m.group(1)
      val index = m.group(2).toInt
      (name, index)
    }

  /** Finds the OFFSET field that corresponds to a storage field.
    *
    * Uses the offset mapping built from <clinit> bytecode to find the correct OFFSET field. Searches in both current
    * class and companion class fields.
    *
    * For bitmap-based lazy vals (3.0-3.2), the OFFSET maps to bitmap field (e.g., "0bitmap$1"). For
    * unsafe/varhandle-based lazy vals (3.3+), the OFFSET maps to storage field (e.g., "a$lzy1").
    *
    * @return
    *   Tuple of (offsetField, location) where location indicates which class contains the field
    */
  private def findOffsetFieldForStorage(
      storageFieldName: String,
      offsetMapping: Map[String, String],
      currentClassFields: Seq[FieldInfo],
      companionClassFields: Seq[FieldInfo]
  ): (Option[FieldInfo], OffsetFieldLocation) =
    // Extract lazy val index from storage field name (e.g., "a$lzy1" -> 1)
    val lazyValIndex = storageFieldPattern.findFirstMatchIn(storageFieldName).map(_.group(2).toInt)

    // Find which OFFSET field maps to this storage field or its associated bitmap field
    val offsetFieldName = offsetMapping.collectFirst {
      // Direct mapping to storage field (3.3+ unsafe/varhandle)
      case (offsetName, mappedStorageName) if mappedStorageName == storageFieldName => offsetName
      // Mapping to bitmap field (3.0-3.2 bitmap-based)
      // Bitmap field is shared by all lazy vals in the class; its index is unrelated to the storage field index
      case (offsetName, mappedName) if mappedName.contains("bitmap") => offsetName
    }

    offsetFieldName match
      case None =>
        (None, OffsetFieldLocation.NoOffsetField)

      case Some(offsetName) =>
        // Search in current class first
        val inCurrentClass = currentClassFields.find { field =>
          field.name == offsetName &&
          field.descriptor == "J" && // long type
          isStaticFinal(field.access)
        }

        inCurrentClass match
          case Some(field) =>
            (Some(field), OffsetFieldLocation.InSameClass)

          case None =>
            // Search in companion class
            val inCompanionClass = companionClassFields.find { field =>
              field.name == offsetName &&
              field.descriptor == "J" && // long type
              isStaticFinal(field.access)
            }

            inCompanionClass match
              case Some(field) =>
                (Some(field), OffsetFieldLocation.InCompanionClass)
              case None =>
                (None, OffsetFieldLocation.NoOffsetField)

  /** Finds the bitmap field that corresponds to a storage field.
    *
    * For 3.0.x-3.2.x, the bitmap field is referenced in <clinit> instead of the lzy field. The bitmap field has the
    * same index as the storage field: "a$lzy1" -> "0bitmap$1"
    */
  private def findBitmapFieldForStorage(
      storageFieldName: String,
      offsetMapping: Map[String, String],
      fields: Seq[FieldInfo]
  ): Option[FieldInfo] =
    // Extract lazy val index from storage field name (e.g., "a$lzy1" -> 1)
    val lazyValIndex = storageFieldPattern.findFirstMatchIn(storageFieldName).map(_.group(2).toInt)

    // Find which OFFSET field maps to a bitmap field with the same index
    val bitmapFieldName = offsetMapping.collectFirst {
      case (offsetName, mappedName) if mappedName.contains("bitmap") => mappedName
    }

    bitmapFieldName.flatMap { bitmapName =>
      fields.find { field =>
        field.name == bitmapName &&
        field.descriptor == "J" && // long type
        !isStatic(field.access) // Instance field
      }
    }

  /** Finds the VarHandle field <name>$lzy<N>$lzyHandle. */
  private def findVarHandleField(
      name: String,
      index: Int,
      fields: Seq[FieldInfo]
  ): Option[FieldInfo] =
    fields.find { field =>
      field.name == s"$name$$lzy$index$$lzyHandle" &&
      field.descriptor == "Ljava/lang/invoke/VarHandle;" &&
      isStaticFinal(field.access) &&
      isPrivate(field.access)
    }

  /** Finds the initialization method <name>$lzyINIT<N>(). */
  private def findInitMethod(
      name: String,
      index: Int,
      methods: Seq[MethodInfo]
  ): Option[MethodInfo] =
    methods.find { method =>
      method.name == s"$name$$lzyINIT$index" &&
      method.descriptor == "()Ljava/lang/Object;"
    }

  /** Finds the accessor method <name>(). */
  private def findAccessorMethod(
      name: String,
      methods: Seq[MethodInfo]
  ): Option[MethodInfo] =
    methods.find(_.name == name)

  /** Determines the Scala version based on field and method patterns. */
  private def determineVersion(
      storageField: FieldInfo,
      offsetField: Option[FieldInfo],
      offsetFieldLocation: OffsetFieldLocation,
      bitmapField: Option[FieldInfo],
      varHandleField: Option[FieldInfo],
      initMethod: Option[MethodInfo],
      accessorMethod: Option[MethodInfo],
      classInfo: ClassInfo,
      companionClassInfo: Option[ClassInfo]
  ): ScalaVersion =
    // First, check for VarHandle (3.8+)
    if varHandleField.isDefined then
      debug("VarHandle field found -> Scala 3.8+")
      return ScalaVersion.Scala38Plus

    // Check for bitmap field (3.0-3.2.x)
    if bitmapField.isDefined then
      debug(s"Bitmap field found: ${bitmapField.get.name} -> Scala 3.0.x-3.2.x")
      // Distinguish between 3.0.x/3.1.x and 3.2.x by checking <clinit>
      // The <clinit> is in the class that contains the OFFSET field
      val clinitClassInfo = offsetFieldLocation match
        case OffsetFieldLocation.InCompanionClass => companionClassInfo.getOrElse(classInfo)
        case _                                    => classInfo

      debug(s"Looking for <clinit> in ${clinitClassInfo.name} (offsetLocation=$offsetFieldLocation)")
      val clinit = clinitClassInfo.methods.find(_.name == "<clinit>")
      clinit match
        case Some(method) =>
          val bytecode = method.bytecodeText
          val hasDeclaredField = bytecode.contains("getDeclaredField")
          val hasGetOffsetStatic = bytecode.contains("getOffsetStatic")
          val hasLazyValsGetOffset = bytecode.contains("LazyVals$.getOffset (")

          debug(
            s"<clinit> checks: getDeclaredField=$hasDeclaredField, getOffsetStatic=$hasGetOffsetStatic, LazyVals.getOffset=$hasLazyValsGetOffset"
          )

          if hasDeclaredField && hasGetOffsetStatic then
            debug("Uses getDeclaredField + getOffsetStatic -> Scala 3.2.x")
            ScalaVersion.Scala32x
          else if hasLazyValsGetOffset then
            debug("Uses LazyVals$.getOffset -> Scala 3.0.x-3.1.x")
            ScalaVersion.Scala30x_31x
          else
            val reason = s"Bitmap field present but unrecognized <clinit> pattern. Bytecode: ${bytecode.take(500)}"
            warn(reason)
            ScalaVersion.Unknown(reason)
        case None =>
          val methods = clinitClassInfo.methods.map(_.name).mkString(", ")
          val reason = s"Bitmap field present but no <clinit> method. Available methods: $methods"
          warn(reason)
          ScalaVersion.Unknown(reason)

    // Check for Object-based with OFFSET (3.3-3.7.x)
    else if offsetField.isDefined &&
      initMethod.isDefined &&
      storageField.descriptor == "Ljava/lang/Object;" &&
      isVolatile(storageField.access)
    then
      // Verify by checking for objCAS usage
      initMethod match
        case Some(method) =>
          if method.bytecodeText.contains("LazyVals$.objCAS") then
            debug("Uses objCAS with Object field -> Scala 3.3.x-3.7.x")
            ScalaVersion.Scala33x_37x
          else
            val reason =
              s"Object field with OFFSET but no objCAS in initMethod ${method.name}. Bytecode: ${method.bytecodeText.take(500)}"
            warn(reason)
            ScalaVersion.Unknown(reason)
        case None =>
          val reason = "Object field with OFFSET but initMethod is None despite isDefined guard"
          warn(reason)
          ScalaVersion.Unknown(reason)
    else
      debug(s"Could not determine version for field ${storageField.name}")
      debug(s"  offsetField=${offsetField.isDefined} initMethod=${initMethod.isDefined}")
      debug(s"  descriptor=${storageField.descriptor} (expected: Ljava/lang/Object;)")
      debug(s"  isVolatile=${isVolatile(storageField.access)}")
      val reason =
        s"No pattern matched. offsetField=${offsetField.isDefined} initMethod=${initMethod.isDefined} descriptor=${storageField.descriptor} isVolatile=${isVolatile(storageField.access)}"
      ScalaVersion.Unknown(reason)

  /** Checks if access flags indicate a static field. */
  private def isStatic(access: Int): Boolean =
    (access & Opcodes.ACC_STATIC) != 0

  /** Checks if access flags indicate a final field. */
  private def isFinal(access: Int): Boolean =
    (access & Opcodes.ACC_FINAL) != 0

  /** Checks if access flags indicate a static final field. */
  private def isStaticFinal(access: Int): Boolean =
    isStatic(access) && isFinal(access)

  /** Checks if access flags indicate a volatile field. */
  private def isVolatile(access: Int): Boolean =
    (access & Opcodes.ACC_VOLATILE) != 0

  /** Checks if access flags indicate a private member. */
  private def isPrivate(access: Int): Boolean =
    (access & Opcodes.ACC_PRIVATE) != 0

object LazyValDetector:
  /** Pattern for lazy val storage fields: <name>$lzy<N> */
  private val storageFieldPattern: Regex = """^(.+)\$lzy(\d+)$""".r

  /** Creates a new detector instance. */
  def apply(): LazyValDetector = new LazyValDetector()

  /** Convenience method to detect lazy vals in a classfile. */
  def detect(classInfo: ClassInfo, companionClassInfo: Option[ClassInfo] = None): LazyValDetectionResult =
    LazyValDetector().detect(classInfo, companionClassInfo)
