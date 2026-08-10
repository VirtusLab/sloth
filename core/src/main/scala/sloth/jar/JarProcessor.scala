package sloth.jar

import sloth.analysis.LazyValAnalyzer
import sloth.patching.BytecodePatcher

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.file.Path
import java.util.jar.{JarEntry, JarInputStream, JarOutputStream, Manifest}
import scala.collection.mutable

object JarProcessor:

  case class JarResult(
      totalClasses: Int,
      patchedClasses: Int,
      failedClasses: Int,
      errors: Seq[String]
  )

  /** Process a JAR: patch all .class entries, write to output path. Non-class entries pass through unchanged. */
  def process(inputJar: Path, outputJar: Path): JarResult =
    val classEntries = mutable.LinkedHashMap[String, Array[Byte]]()
    val nonClassEntries = mutable.LinkedHashMap[String, Array[Byte]]()
    var manifest: Option[Manifest] = None

    // Read all entries from input JAR
    val jis = new JarInputStream(java.nio.file.Files.newInputStream(inputJar))
    try
      manifest = Option(jis.getManifest)
      var entry = jis.getNextJarEntry
      while entry != null do
        val name = entry.getName
        if !entry.isDirectory then
          val bytes = readAllBytes(jis)
          if name.endsWith(".class") then classEntries(name) = bytes
          else nonClassEntries(name) = bytes
        entry = jis.getNextJarEntry
    finally jis.close()

    // Convert entry paths to dotted class names for the analyzer
    val classfileMap: Map[String, Array[Byte]] = classEntries.map { (entryPath, bytes) =>
      val className = entryPath.stripSuffix(".class").replace('/', '.')
      className -> bytes
    }.toMap

    // Build reverse mapping: dotted name -> entry path
    val nameToEntryPath: Map[String, String] = classEntries.keys.map { entryPath =>
      val className = entryPath.stripSuffix(".class").replace('/', '.')
      className -> entryPath
    }.toMap

    // Build a classloader that can resolve classes from the JAR
    val jarClassLoader = new java.net.URLClassLoader(
      Array(inputJar.toUri.toURL),
      getClass.getClassLoader
    )

    // Group and patch
    val errors = mutable.ArrayBuffer[String]()
    val patchedBytes = mutable.Map[String, Array[Byte]]() // keyed by entry path

    LazyValAnalyzer.group(classfileMap) match
      case Left(error) =>
        errors += s"Failed to group classfiles: $error"

      case Right(groups) =>
        for group <- groups do
          try
            BytecodePatcher.patch(group, classLoader = Some(jarClassLoader)) match
              case BytecodePatcher.PatchResult.PatchedSingle(name, bytes) =>
                nameToEntryPath.get(name).foreach(ep => patchedBytes(ep) = bytes)

              case BytecodePatcher.PatchResult.PatchedPair(objName, clsName, objBytes, clsBytes) =>
                nameToEntryPath.get(objName).foreach(ep => patchedBytes(ep) = objBytes)
                nameToEntryPath.get(clsName).foreach(ep => patchedBytes(ep) = clsBytes)

              case BytecodePatcher.PatchResult.NotApplicable => // nothing to do
              case BytecodePatcher.PatchResult.Failed(error) =>
                errors += s"Failed to patch group ${group.primaryName}: $error"
          catch
            case e: Exception =>
              errors += s"Exception patching group ${group.primaryName}: ${e.getMessage}"

    // Write output JAR
    val jos = manifest match
      case Some(m) => new JarOutputStream(java.nio.file.Files.newOutputStream(outputJar), m)
      case None    => new JarOutputStream(java.nio.file.Files.newOutputStream(outputJar))
    try
      // Write class entries (patched or original)
      for (entryPath, originalBytes) <- classEntries do
        val je = new JarEntry(entryPath)
        jos.putNextEntry(je)
        jos.write(patchedBytes.getOrElse(entryPath, originalBytes))
        jos.closeEntry()

      // Write non-class entries verbatim
      for (entryPath, bytes) <- nonClassEntries do
        val je = new JarEntry(entryPath)
        jos.putNextEntry(je)
        jos.write(bytes)
        jos.closeEntry()
    finally jos.close()

    JarResult(
      totalClasses = classEntries.size,
      patchedClasses = patchedBytes.size,
      failedClasses = errors.size,
      errors = errors.toSeq
    )

  private def readAllBytes(is: InputStream): Array[Byte] =
    val buf = new Array[Byte](8192)
    val out = new ByteArrayOutputStream()
    var n = is.read(buf)
    while n != -1 do
      out.write(buf, 0, n)
      n = is.read(buf)
    out.toByteArray
