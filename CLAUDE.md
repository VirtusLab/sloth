# Claude Code Development Guide

This document contains information for Claude Code (or other AI assistants) working on the lazyvalgrade project.

## Project Structure

- **core/** - Core bytecode analysis and transformation logic (published; `-release 9`)
- **agent/** - Java agent that patches lazy vals at class-load time (published, shaded fat jar; `-release 9`)
- **cli/** - Command-line interface for running transformations (`-release 9`)
- **testops/** - Development tooling + shared test infra (`ExampleLoader`, `ExampleRunner`, `TestPaths`)
- **tests/** - Test fixtures only (`src/test/resources/fixtures/examples/`); no sbt module
- **tests-jdk9/** - Test suites that run on Java 9–11 (analysis + Java-9 runtime/classfile-version proof)
- **tests-jdk25/** - Test suites that run on Java 24+ (sun.misc.Unsafe warning assertions)

## Development Tools

### Compiling Test Examples

The `compileExamples` command compiles all test fixtures across multiple Scala versions and generates javap outputs for bytecode inspection.

**Usage:**

```bash
# Compile examples without patching
sbt compileExamples

# Compile examples and generate patched versions (3.3-3.7)
sbt compileExamplesWithPatching

# Run with example filtering
SELECT_EXAMPLE=simple-lazy-val sbt compileExamples
SELECT_EXAMPLE=simple-lazy-val,class-lazy-val sbt compileExamplesWithPatching

# Or run the assembly directly
sbt testops/assembly
java -jar testops/target/scala-3.3.8/lazyvalgrade-testops.jar
java -jar testops/target/scala-3.3.8/lazyvalgrade-testops.jar --patch
```

**What it does:**

1. Discovers all examples in `tests/src/test/resources/fixtures/examples/`
2. Compiles each example with all test Scala versions:
   - 3.0.2, 3.1.3, 3.2.2
   - 3.3.0, 3.3.6, 3.4.3, 3.5.2, 3.6.4, 3.7.3
   - 3.8.1
3. Generates javap disassembly (`.javap.txt`) for each compiled classfile
4. With `--patch` flag: Transforms Scala 3.3-3.7 classfiles to use VarHandle-based lazy vals (like 3.8+)
5. Outputs everything to `.out/` directory

**Output structure:**

```
.out/
  <example-name>/
    <scala-version>/
      *.class           # Compiled classfiles
      *.javap.txt       # Javap disassembly
      *.scala          # Source files (copied)
      .scala-build/    # scala-cli build artifacts
    patched/           # Only present when using --patch flag
      3.3.0/           # Patched versions (3.3-3.7 only)
        *.class        # Patched classfiles with VarHandle-based lazy vals
        *.javap.txt    # Javap disassembly of patched files
      3.3.6/
      3.4.3/
      3.5.2/
      3.6.4/
      3.7.3/
```

**Inspecting results:**

```bash
# List all examples
ls .out/

# View javap output for a specific version
cat .out/companion-object-lazy-val/3.3.0/Foo$.javap.txt

# View patched javap output
cat .out/companion-object-lazy-val/patched/3.3.0/Foo$.javap.txt

# Compare lazy val implementations across versions
grep -h "OFFSET\|bitmap\|lzyHandle" .out/simple-lazy-val/*/SimpleLazyVal$.javap.txt

# Compare original vs patched (OFFSET -> VarHandle)
diff .out/simple-lazy-val/3.3.0/SimpleLazyVal$.javap.txt .out/simple-lazy-val/patched/3.3.0/SimpleLazyVal$.javap.txt

# Count generated files
find .out -name "*.javap.txt" | wc -l
```

**Use cases:**

- Debugging lazy val detection across Scala versions
- Comparing bytecode patterns between versions
- Verifying transformation correctness
- Understanding lazy val implementation changes
- Testing bytecode patching by comparing original vs patched implementations
- Inspecting VarHandle vs Unsafe-based lazy val bytecode

## Testing

### Tests are split by required JVM — `sbt test` is disabled

Scala 3.8 requires JDK 17 and can't emit bytecode below v61, so this build uses **Scala 3.3.8** (LTS,
Java-8 stdlib) with `-Yfuture-lazy-vals` (keeps the Unsafe-free VarHandle lazy-val scheme) and
`-release 9` on the published modules (`core`, `agent`, `cli`). Artifacts are therefore loadable on
Java 9. The test suites are split into two sbt modules by the JVM they need, and a plain `sbt test`
is intentionally disabled (it errors with a pointer to the two targets):

- **`sbt tests-jdk9`** (module `tests-jdk9`, run on **Java 11**) — pure bytecode-analysis suites
  (`LazyValDetectionTests`, `SemanticLazyValComparisonTests`, `AgentPatchingTests`), the Java-9
  runtime proof (`Jdk9RuntimeTests` runs the agent + VarHandle-patched 3.3–3.7 code), and
  `ClassfileVersionTests` (asserts the agent jar + core are ≤ v53). Locally there's no JDK 9; use
  `JAVA_HOME=~/.sdkman/candidates/java/11.0.31-tem` (Java 11 is the oldest sbt-friendly proxy).
- **`sbt tests-jdk25`** (module `tests-jdk25`, run on **Java 24+**) — `BytecodePatchingTests` and
  `AgentIntegrationTests`, which assert presence/absence of the `sun.misc.Unsafe` warning that only
  newer JDKs emit. Locally: `JAVA_HOME=~/.sdkman/candidates/java/25-graalce`.

CI runs these as two jobs (`test-jdk9` on temurin 11, `test-jdk25` on temurin 25). The harness
itself needs Java 11+ (sbt, scala-cli, and testops' jsoniter dependency are >v53), so the actual
Java-9 floor is guaranteed by `ClassfileVersionTests` (published classes ≤ v53), not by executing
on Java 9. Java 9/10 are best-effort.

**IMPORTANT: still narrow with `SELECT_EXAMPLE` / `ONLY_SCALA_VERSIONS`.** A full module run compiles
examples across 10+ Scala versions and is very slow. To verify a full module passes, ask the user.

The `sbt test` invocations in the filtering examples below are illustrative — substitute
`tests-jdk9` or `tests-jdk25` (or `testsJdk9/testOnly ...`) for the suite you're targeting.

```bash
# Run a specific suite with filtering (preferred). Detection lives in tests-jdk9:
SELECT_EXAMPLE=simple-lazy-val sbt "testsJdk9/testOnly lazyvalgrade.LazyValDetectionTests"

# A single example's runtime behaviour (warning suite) lives in tests-jdk25:
SELECT_EXAMPLE=companion-object-lazy-val sbt "testsJdk25/testOnly lazyvalgrade.BytecodePatchingTests"
```

### Filtering Examples and Scala Versions

All test suites support filtering using environment variables:

#### SELECT_EXAMPLE - Filter by example name

```bash
# Run tests for a single example
SELECT_EXAMPLE=simple-lazy-val sbt test

# Run tests for multiple examples (comma-separated)
SELECT_EXAMPLE=simple-lazy-val,class-lazy-val sbt test

# Run specific test suite with filtering
SELECT_EXAMPLE=companion-object-lazy-val sbt "testsJdk25/testOnly lazyvalgrade.BytecodePatchingTests"

# Without SELECT_EXAMPLE, all examples are tested (default behavior)
sbt test
```

#### ONLY_SCALA_VERSIONS - Filter by Scala version

```bash
# Test only specific Scala versions
ONLY_SCALA_VERSIONS=3.1.3,3.3.0 sbt test

# Combine with example filtering for targeted testing
SELECT_EXAMPLE=simple-lazy-val ONLY_SCALA_VERSIONS=3.3.0,3.4.3 sbt test

# Test a problematic version in isolation
ONLY_SCALA_VERSIONS=3.3.0 sbt "testsJdk9/testOnly lazyvalgrade.LazyValDetectionTests"
```

#### INSPECT_BYTECODE - Enable bytecode inspection on test failures

When enabled, this mode automatically prints `javap -v -p` output for failing test cases,
showing the failed version plus adjacent versions for comparison.

```bash
# Enable bytecode inspection for all test failures
INSPECT_BYTECODE=true sbt test

# Combine all filters for precise debugging
INSPECT_BYTECODE=true SELECT_EXAMPLE=multiple-lazy-vals ONLY_SCALA_VERSIONS=3.1.3,3.3.0 sbt test

# Debug a specific test with full bytecode output
INSPECT_BYTECODE=1 SELECT_EXAMPLE=simple-lazy-val ONLY_SCALA_VERSIONS=3.3.0 \
  sbt "testsJdk9/testOnly lazyvalgrade.LazyValDetectionTests"
```

**INSPECT_BYTECODE accepts:** `true`, `1`, `yes` (case insensitive)

**Available examples:**
- `simple-lazy-val` - Basic lazy val in object
- `class-lazy-val` - Lazy val in class
- `companion-object-lazy-val` - Lazy val in companion object
- `companion-class-lazy-val` - Lazy val in companion class
- `multiple-lazy-vals` - Multiple lazy vals in single object
- `abstract-class-lazy-val` - Lazy val in abstract class
- `trait-class-lazy-val` - Lazy val in trait
- `no-lazy-val` - Control case with no lazy vals

**Use cases:**
- Faster iteration when working on specific examples
- Debugging issues in particular test cases
- CI optimization by parallelizing example tests

## Building

```bash
# Compile all modules
sbt compile

# Build CLI assembly
sbt cli/assembly
# Output: cli/target/scala-3.3.8/lazyvalgrade.jar

# Build testops assembly
sbt testops/assembly
# Output: testops/target/scala-3.3.8/lazyvalgrade-testops.jar
```

## Important Notes

### Lazy Val Detection

The project detects and transforms lazy val implementations across different Scala 3 versions:

- **3.0.x - 3.2.x**: Bitmap-based with typed storage fields
- **3.3.x - 3.7.x**: OFFSET-based with Object storage and objCAS
- **3.8.x+**: VarHandle-based with Object storage

### Test Fixtures

Test fixtures are located in `tests/src/test/resources/fixtures/examples/`:
- `simple-lazy-val/` - Basic lazy val in object
- `class-lazy-val/` - Lazy val in class
- `companion-object-lazy-val/` - Lazy val in companion object
- `companion-class-lazy-val/` - Lazy val in companion class
- `no-lazy-val/` - Control case with no lazy vals

Each fixture includes a `metadata.json` file describing expected lazy val patterns.

### Test Output Control

Test suites support a `quietTests` flag to control verbosity. All test suites are **quiet by default** (minimal output).

To enable verbose output for debugging, override in specific test suites:
```scala
override val quietTests: Boolean = false  // Enable verbose output
```

### Error Handling: Hard-fail on Unknown Detection

`ScalaVersion.Unknown(reason: String)` carries a diagnostic reason explaining which detection heuristic failed. When the agent encounters an `Unknown` lazy val (or `MixedVersions`), it throws `LazyValPatchingException` with a full diagnostic dump (class fields, methods, per-lazy-val breakdown) instead of silently skipping. This ensures broken Unsafe-based lazy vals don't silently cause `VerifyError` at runtime on newer JDKs.

New test fixtures should be added for any class that triggers `Unknown` detection — the diagnostic output is designed to provide all the info needed to reproduce the case.

### Known Issues

- Test failures can be intermittent (see previous session notes)
- Some race conditions in parallel compilation/detection
- VarHandle OFFSET field detection differs between standalone and companion cases

## Debugging Tips

### Quick Debugging Workflow

When a test fails, use this workflow for maximum debugging velocity:

```bash
# 1. Enable bytecode inspection and narrow down to the failing case
INSPECT_BYTECODE=true SELECT_EXAMPLE=<failing-example> ONLY_SCALA_VERSIONS=<failing-version> sbt test

# 2. The test will automatically print javap output for the failed version and adjacent versions

# 3. For deeper analysis, compile the examples separately and inspect manually
SELECT_EXAMPLE=<example> sbt compileExamples
cat .out/<example>/<version>/<ClassName>.javap.txt

# 4. Compare bytecode across versions
diff .out/<example>/3.3.0/<Class>.javap.txt .out/<example>/3.4.3/<Class>.javap.txt
```

### General Tips

1. Use `compileExamples` to generate fresh bytecode when behavior seems inconsistent
2. Use `INSPECT_BYTECODE=true` to automatically see bytecode on test failures
3. Use `ONLY_SCALA_VERSIONS` to test specific problematic versions in isolation
4. Combine all three environment variables for pinpoint debugging
5. Check `.out/` directory structure when tests fail to verify compilation succeeded
6. Use `javap -v -p` manually for deeper inspection of specific classfiles

### Example Debugging Session

```bash
# Start with a broad test to identify failures
sbt test

# Test fails on multiple-lazy-vals with Scala 3.3.0
# Narrow down and inspect bytecode
INSPECT_BYTECODE=true SELECT_EXAMPLE=multiple-lazy-vals ONLY_SCALA_VERSIONS=3.1.3,3.3.0 sbt test

# The output will show javap for both versions, making it easy to spot differences
# in lazy val implementation (bitmap vs OFFSET-based)
```
