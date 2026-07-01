package sloth

import munit.FunSuite
import sloth.agent.{SlothTransformer, AgentConfig}
import sloth.classfile.ClassfileParser
import sloth.lazyval.SemanticLazyValComparator
import java.nio.file.Files
import java.io.ByteArrayInputStream

/** Tests for SlothTransformer — the agent's ClassFileTransformer that uses
  * group-based patching via BytecodePatcher.patch(group).
  *
  * Verifies that the transformer correctly patches lazy val bytecode when classes
  * are loaded through a MockClassLoader, including companion pair handling where
  * loading order may vary.
  */
class AgentPatchingTests extends FunSuite with ExampleLoader {

  override val munitTimeout = scala.concurrent.duration.Duration(600, "s")

  override val examplesDir: os.Path = os.pwd / "tests" / "src" / "test" / "resources" / "fixtures" / "examples"
  override val testWorkspace: os.Path = os.temp.dir(prefix = "sloth-agent-tests-", deleteOnExit = false)
  override val quietCompilation: Boolean = true
  override val quietTests: Boolean = true

  val testVersions: Seq[String] = Seq(
    "3.0.2", "3.1.3", "3.2.2",
    "3.3.0", "3.3.6", "3.4.3", "3.5.2", "3.6.4", "3.7.3",
    "3.8.1"
  )

  override def requiredScalaVersions: Seq[String] = testVersions

  val patchableVersions: Set[String] = Set(
    "3.0.2", "3.1.3", "3.2.2",
    "3.3.0", "3.3.6", "3.4.3", "3.5.2", "3.6.4", "3.7.3"
  )

  private def log(msg: => String): Unit = if !quietTests then println(msg)

  lazy val examples: Seq[LoadedExample] = {
    log(s"Examples directory: $examplesDir")
    log(s"Test workspace: $testWorkspace")
    val loaded = loadSelectedExamples()
    log(s"Loaded ${loaded.size} examples for testing")
    loaded
  }

  override def beforeAll(): Unit = {
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(minimumLevel = Some(scribe.Level.Warn))
      .replace()
  }

  /** ClassLoader that serves class bytes from an in-memory map.
    * Keys are resource names like "Foo$.class".
    */
  class MockClassLoader(classes: Map[String, Array[Byte]]) extends ClassLoader(null) {
    override def getResourceAsStream(name: String): java.io.InputStream =
      classes.get(name).map(bytes => new ByteArrayInputStream(bytes)).orNull
  }

  /** Build a map of internalName -> bytes from an example's compilation result for a given version. */
  def buildClassBytesMap(example: LoadedExample, version: String): Map[String, Array[Byte]] = {
    val vr = example.compilationResult.results(version)
    vr.classFiles.map { cf =>
      val internalName = cf.relativePath.stripSuffix(".class")
      val bytes = Files.readAllBytes(cf.absolutePath.toNIO)
      (internalName, bytes)
    }.toMap
  }

  /** Simulate class loading through the transformer in a given order.
    *
    * @param transformer fresh transformer instance (stateful due to companion buffer)
    * @param classBytesMap internalName -> original bytes
    * @param loadOrder sequence of internal names in desired loading order
    * @return map of internalName -> patched bytes (only classes that were actually patched)
    */
  def simulateLoading(
      transformer: SlothTransformer,
      classBytesMap: Map[String, Array[Byte]],
      loadOrder: Seq[String]
  ): Map[String, Array[Byte]] = {
    val mockLoader = new MockClassLoader(classBytesMap.map { case (k, v) => (k + ".class") -> v })
    loadOrder.flatMap { internalName =>
      val result = transformer.transform(mockLoader, internalName, null, null, classBytesMap(internalName))
      if (result != null) Some(internalName -> result) else None
    }.toMap
  }

  test("agent patches patchable classes for all versions") {
    examples.foreach { example =>
      if (example.metadata.expectedClasses.exists(_.lazyVals.nonEmpty)) {
        log(s"\n[${example.name}] Testing agent transformer patching")

        val compiledPatchableVersions = patchableVersions.filter(v =>
          example.compilationResult.results.get(v).exists(_.success))

        // Get 3.8 reference bytes for semantic comparison
        val ref38Result = example.compilationResult.results.get("3.8.1").filter(_.success)

        compiledPatchableVersions.foreach { version =>
          val classBytesMap = buildClassBytesMap(example, version)
          val transformer = new SlothTransformer(AgentConfig())
          val allNames = classBytesMap.keys.toSeq
          val patched = simulateLoading(transformer, classBytesMap, allNames)

          assert(patched.nonEmpty, s"[${example.name}/$version] At least one class should be patched")

          patched.foreach { case (internalName, patchedBytes) =>
            // Verify patched bytes are parseable
            val parsed = ClassfileParser.parse(patchedBytes)
            assert(parsed.isRight,
              s"Patched classfile should be parseable for $internalName ($version): ${parsed.left.getOrElse("")}")

            // Semantic comparison with 3.8 reference if available
            ref38Result.foreach { ref38 =>
              val className = internalName.stripSuffix("$")
              val refClassFile = ref38.classFiles.find(_.relativePath.stripSuffix(".class") == internalName)
              refClassFile.foreach { refCf =>
                val refBytes = Files.readAllBytes(refCf.absolutePath.toNIO)
                val refClassInfo = ClassfileParser.parse(refBytes).toOption.get
                val patchedClassInfo = parsed.toOption.get
                val comparison = SemanticLazyValComparator.compare(patchedClassInfo, refClassInfo)
                assert(comparison.areIdentical,
                  s"[${example.name}/$version] Patched $internalName should be semantically identical to 3.8, but got: $comparison")
              }
            }

            log(s"  [$version] $internalName: Patched (${classBytesMap(internalName).length} -> ${patchedBytes.length} bytes)")
          }
        }
      }
    }
  }

  test("companion pair patching works regardless of loading order") {
    // Test that loading companion classes in different orders produces correct results.
    // Focus on examples where companion pairing matters for lazy val patching.
    examples.foreach { example =>
      if (example.metadata.expectedClasses.exists(_.lazyVals.nonEmpty)) {
        val compiledPatchableVersions = patchableVersions.filter(v =>
          example.compilationResult.results.get(v).exists(_.success))

        compiledPatchableVersions.foreach { version =>
          val classBytesMap = buildClassBytesMap(example, version)
          val allNames = classBytesMap.keys.toSeq

          // Find companion pairs: names where both "X" and "X$" exist
          val objectNames = allNames.filter(_.endsWith("$"))
          val companionPairs = objectNames.flatMap { objName =>
            val clsName = objName.stripSuffix("$")
            if (classBytesMap.contains(clsName)) Some((clsName, objName)) else None
          }

          if (companionPairs.nonEmpty) {
            companionPairs.foreach { case (clsName, objName) =>
              // Order 1: class first, then object
              val transformer1 = new SlothTransformer(AgentConfig())
              val order1 = Seq(clsName, objName) ++ allNames.filterNot(n => n == clsName || n == objName)
              val patched1 = simulateLoading(transformer1, classBytesMap, order1)

              // Order 2: object first, then class
              val transformer2 = new SlothTransformer(AgentConfig())
              val order2 = Seq(objName, clsName) ++ allNames.filterNot(n => n == clsName || n == objName)
              val patched2 = simulateLoading(transformer2, classBytesMap, order2)

              // Only test pairs where at least one order actually patches something
              // (some X/X$ pairs have no lazy vals, e.g., App/App$ when lazy vals
              // are in anonymous inner classes)
              val union = patched1.keySet ++ patched2.keySet
              val pairRelated = union.intersect(Set(clsName, objName))

              if (pairRelated.nonEmpty) {
                log(s"\n[${example.name}/$version] Testing loading order for $clsName/$objName")

                // For classes patched in both orders, verify semantic equivalence to 3.8
                val ref38Result = example.compilationResult.results.get("3.8.1").filter(_.success)
                ref38Result.foreach { ref38 =>
                  pairRelated.foreach { name =>
                    val refCf = ref38.classFiles.find(_.relativePath.stripSuffix(".class") == name)
                    refCf.foreach { cf =>
                      val refBytes = Files.readAllBytes(cf.absolutePath.toNIO)
                      val refClassInfo = ClassfileParser.parse(refBytes).toOption.get

                      Seq(("class-first", patched1), ("object-first", patched2)).foreach { case (label, patched) =>
                        patched.get(name).foreach { patchedBytes =>
                          val patchedClassInfo = ClassfileParser.parse(patchedBytes).toOption.get
                          val comparison = SemanticLazyValComparator.compare(patchedClassInfo, refClassInfo)
                          assert(comparison.areIdentical,
                            s"[${example.name}/$version] $label order: $name should be semantically identical to 3.8, but got: $comparison")
                        }
                      }
                    }
                  }
                }

                log(s"  [$version] $clsName/$objName: Both orders produce correct results")
              }
            }
          }
        }
      }
    }
  }

  test("returns null for 3.8+ classes") {
    examples.foreach { example =>
      if (example.metadata.expectedClasses.exists(_.lazyVals.nonEmpty)) {
        log(s"\n[${example.name}] Testing 3.8+ returns null")

        example.compilationResult.results.get("3.8.1").foreach { versionResult =>
          if (versionResult.success) {
            val classBytesMap = buildClassBytesMap(example, "3.8.1")
            val transformer = new SlothTransformer(AgentConfig())
            val allNames = classBytesMap.keys.toSeq
            val patched = simulateLoading(transformer, classBytesMap, allNames)

            assert(patched.isEmpty,
              s"[${example.name}] 3.8+ classes should all return null (not be patched), but got: ${patched.keySet}")
            log(s"  [3.8.1] All classes returned null (as expected)")
          }
        }
      }
    }
  }

  test("returns null for non-lazy-val classes") {
    examples.foreach { example =>
      if (!example.metadata.expectedClasses.exists(_.lazyVals.nonEmpty)) {
        log(s"\n[${example.name}] Testing non-lazy-val classes return null")

        val someVersion = example.compilationResult.successfulResults.keys.headOption
        someVersion.foreach { version =>
          val classBytesMap = buildClassBytesMap(example, version)
          val transformer = new SlothTransformer(AgentConfig())
          val allNames = classBytesMap.keys.toSeq
          val patched = simulateLoading(transformer, classBytesMap, allNames)

          assert(patched.isEmpty,
            s"[${example.name}/$version] Non-lazy-val classes should all return null, but got: ${patched.keySet}")
          log(s"  [$version] All classes returned null (as expected)")
        }
      }
    }
  }

  test("patching is idempotent") {
    examples.foreach { example =>
      if (example.metadata.expectedClasses.exists(_.lazyVals.nonEmpty)) {
        log(s"\n[${example.name}] Testing idempotency")

        val firstPatchableVersion = patchableVersions
          .filter(v => example.compilationResult.results.get(v).exists(_.success))
          .toSeq.sorted.headOption

        firstPatchableVersion.foreach { version =>
          val classBytesMap = buildClassBytesMap(example, version)
          val transformer1 = new SlothTransformer(AgentConfig())
          val allNames = classBytesMap.keys.toSeq
          val patched = simulateLoading(transformer1, classBytesMap, allNames)

          if (patched.nonEmpty) {
            // Build a new class bytes map with patched bytes replacing originals
            val patchedClassBytesMap = classBytesMap.map { case (name, originalBytes) =>
              name -> patched.getOrElse(name, originalBytes)
            }

            // Feed through a fresh transformer — everything should return null
            val transformer2 = new SlothTransformer(AgentConfig())
            val repatched = simulateLoading(transformer2, patchedClassBytesMap, allNames)

            assert(repatched.isEmpty,
              s"[${example.name}/$version] Re-patching should return null for all classes, but got: ${repatched.keySet}")
            log(s"  [$version] Idempotent (all return null on re-patch)")
          }
        }
      }
    }
  }
}
