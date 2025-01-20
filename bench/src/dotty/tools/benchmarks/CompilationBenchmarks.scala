package dotty.tools.benchmarks

import java.util.concurrent.TimeUnit.MILLISECONDS
import java.io.File

import scala.sys.process.{ProcessBuilder, stringToProcess}

import org.openjdk.jmh.annotations.{Fork, Warmup, Measurement, BenchmarkMode, Benchmark, State, Setup, Scope, Level, OutputTimeUnit}

import dotty.tools.dotc.{Driver, Run, Compiler}
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Types.{TermRef, Type}
import dotty.tools.dotc.core.Contexts.{ContextBase, Context, ctx, withMode}

/** Compilation benchmarks.
  *
  * Results over time can be visualized at https://dotty-bench.epfl.ch.
  *
  * Pre-requisites: the benchmarks initialization runs the `cs` (coursier), `rm`
  * and `mkdir` commands. It is only expected to work on Unix-like systems.
  *
  * The `CompilationBenchmarks` class below is processed and run by JMH.
  *
  * To run all compilation benchmarks, run the following command from SBT:
  * ```
  * scala3-bench / Jmh / run CompilationBenchmarks -foe true
  * ```
  *
  * Each benchmark is a method annotated with `@Benchmark`.
  *
  * Benchmarks that should only be run nightly contains `Nightly` in their name.
  * Other benchmarks are run for each merged PR.
  *
  * Benchmarks that should be run using the bootstrapped compiler contains
  * `Bootstrapped` in their name. Other benchmarks are run using the
  * non-bootstrapped compiler.
  *
  * For example, to run only non-bootstrapped "on-merge" benchmarks—those that
  * do _not_ run nightly, and that do not use the bootstrapped compiler—, you
  * can use the following command (`-e` is for excluding):
  *
  * ```
  * scala3-bench / Jmh / run -foe true -e Nightly -e Bootstrapped
  * ```
  *
  * `-foe true` means "fail on error". This will stop the benchmark run if any
  * benchmark fails.
  *
  * To run only benchmarks that should be run nightly, you can use the following
  * command:
  * ```
  * scala3-bench / Jmh / run -foe true Nightly
  * ```
  *
  * You can also filter by other parts of the (fully-qualified) benchmark method
  * name. For example, to only run the `dottyNightly` benchmark, you can use:
  * ```
  * scala3-bench / Jmh / run -foe true CompilationBenchmarks.dotty
  * ```
  *
  * To test benchmarks quickly, you can override the number of warmup and
  * measurement iterations respectively using the `-wi` and `-i` options:
  * ```
  * scala3-bench / Jmh / run -foe true -wi 1 -i 2 CompilationBenchmarks.implicitNumsTasty
  * ```
  *
  * Use `-help` to list all JMH options:
  * ```
  * scala3-bench / Jmh / run -help
  * ```
  *
  * The benchmarks in `tests/bench` are also run as part of normal tests. You
  * can run these with:
  * ```
  * scala3-compiler-bootstrapped / testOnly dotty.tools.dotc.BootstrappedOnlyCompilationTests -- *posBenchs*
  * ```
  *
  * Important: the `@Benchmark` methods, must be kept minimal to avoid any side
  * effects that could affect the benchmark results.
  *
  * Useful references:
  *   - JMH examples:
  *     https://github.com/openjdk/jmh/tree/master/jmh-samples/src/main/java/org/openjdk/jmh/samples
  *   - JMH annotations documentation:
  *     https://javadoc.io/doc/org.openjdk.jmh/jmh-core/1.1.1/org/openjdk/jmh/annotations/package-summary.html
  */
@Fork(value = 1, jvmArgsPrepend = Array("-XX:+PrintCommandLineFlags", "-Xms2G", "-Xmx2G"))
@Warmup(iterations = 150) // default, overridden below for some benchmarks
@Measurement(iterations = 30) // default, overridden below for some benchmarks
@BenchmarkMode(Array(org.openjdk.jmh.annotations.Mode.SingleShotTime))
@State(Scope.Benchmark)
@OutputTimeUnit(MILLISECONDS)
class CompilationBenchmarks:
  /** Temporary output directory for compilation. Deleted between iterations. */
  val TMP_DIR = "tmp"

  /** Output directory for files to be kept. This is used by benchmarks that
    * compile from TASTy, such as `implicitCacheTasty`.
    */
  val OUT_DIR = "out"

  /** Directory in which to generate synthetic benchmarks. */
  val GENERATED_DIR = "tests-generated"

  /** Launches `scalac` with the given arguments. */
  def compile(args: Array[String], out_dir: String = TMP_DIR) =
    // Run benchmark with the default benchmarks classpath if not specified.
    val classPathArg = if args.contains("-classpath") then Array.empty[String] else Array("-classpath", benchClassPath)
    val allArgs = Array("-Xfatal-warnings", "-d", out_dir) ++ classPathArg ++ args
    val reporter = Driver().process(allArgs)
    assert(!reporter.hasErrors, "Compilation failed with errors")

  /** Finds files in `d` and its subdirectories that satisfy `p`. */
  def find(d: String, p: String => Boolean): Array[String] =
    def rec(d: File, p: String => Boolean): Array[String] =
      val children = d.listFiles
      children.map(_.toString).filter(p) ++ children.filter(_.isDirectory).flatMap(rec(_, p))
    val f = File(d)
    if !f.exists then throw new IllegalArgumentException(s"Directory ${f.getAbsolutePath()} does not exist")
    rec(f, p)

  /** Finds files with the `scala` extension in `d` and its subdirectories. */
  def findScalaFiles(d: String) = find(d, _.endsWith(".scala"))

  /** Get the value of a system property, fail if it is not set. */
  def getDefinedProperty(name: String) =
    val res = System.getProperty(name)
    assert(res != null, s"$name must be set")
    res

  /** Removes and creates a directory. */
  def removeAndCreateDir(dir: String) =
    // Using `rm` instead of Java's API because it is better at removing the
    // whole directory atomically. Got occasional `DirectoryNotEmptyException`
    // exceptions with the Java's API.
    s"rm -rf $dir".!
    s"mkdir -p $dir".!

  /** Class path of `scala3-library-bootstrapped`. Defined in `Build.scala`. */
  val benchClassPath = getDefinedProperty("BENCH_CLASS_PATH")

  /** Class path of `scala3-bootstrapped`. Defined in `Build.scala`. */
  val benchCompilerClassPath = getDefinedProperty("BENCH_COMPILER_CLASS_PATH")


  @Setup(Level.Trial)
  def trialSetup(): Unit =
    removeAndCreateDir(OUT_DIR)

    // Some benchmarks use TASTy or class files compiled beforehand. These are
    // compiled here.
    removeAndCreateDir(implicitCacheOut)
    compile(implicitCacheArgs, implicitCacheOut)
    removeAndCreateDir(implicitNumsOut)
    compile(implicitNumsArgs, implicitNumsOut)
    removeAndCreateDir(powerMacroOut)
    compile(Array(powerMacroSource), powerMacroOut)
    removeAndCreateDir(stringInterpolationMacroOut)
    compile(Array(stringInterpolationMacroSource), stringInterpolationMacroOut)

    // Generates benchmarks in `tests-generated`. These are big (>500 kB) source
    // files that are probably better generated on the fly than stored in the
    // repository.
    generateBenchmarks(GENERATED_DIR)

  @Setup(Level.Iteration)
  def setup(): Unit =
    s"rm -rf $TMP_DIR".!
    s"mkdir -p $TMP_DIR".!

  val dottyArgs =
    val sources = find("../compiler/src/dotty", f => f.endsWith(".scala") || f.endsWith(".java"))
    // format: off
    Array(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-encoding", "UTF8",
      "-language:implicitConversions",
      "-Yexplicit-nulls",
      "-Wsafe-init",
      "-source", "3.3",
      "-classpath", benchCompilerClassPath
    ) ++ sources
    // format: on

  @Warmup(iterations = 10)
  @Measurement(iterations = 6)
  @Benchmark
  def dottyNightly() = compile(dottyArgs)

  val stdlibArgs =
    val sources = find(
      "../community-build/community-projects/stdLib213/src/library/scala",
      f => (f.endsWith(".scala") || f.endsWith(".java")) && !f.endsWith("language.scala") && !f.endsWith("AnyVal.scala")
    )
    Array("-language:implicitConversions", "-Wconf:any:s", "-source", "3.3") ++ sources

  @Warmup(iterations = 25)
  @Measurement(iterations = 10)
  @Benchmark
  def stdlibNightly() = compile(stdlibArgs)

  val scalapArgs =
    val sources = findScalaFiles("../community-build/community-projects/scalap/src/scalap")
    val depClasspath = "cs fetch -p org.scala-lang:scala-compiler:2.13.0".!!
    Array("-source", "3.0-migration", "-Wconf:any:s", "-classpath", s"$depClasspath:$benchClassPath") ++ sources

  @Benchmark def scalap() = compile(scalapArgs)

  val re2sArgs = Array("-Wconf:any:s") ++ findScalaFiles("../tests/bench/re2s/src")
  @Benchmark def re2s() = compile(re2sArgs)

  val implicitCacheArgs = Array("../tests/bench/implicit_cache.scala")
  @Benchmark def implicitCacheBootstrappedNightly() = compile(implicitCacheArgs)

  val implicitCacheOut = s"$OUT_DIR/implicit_cache"
  val implicitCacheTastyArgs = Array(
    "-from-tasty",
    s"$implicitCacheOut/Test.tasty",
    s"$implicitCacheOut/A.tasty",
    s"$implicitCacheOut/Foo.tasty"
  )
  @Benchmark def implicitCacheTastyNightly() = compile(implicitCacheTastyArgs)

  val implicitNumsOut = s"$OUT_DIR/implicitNums"
  val implicitNumsArgs = Array("../tests/bench/implicitNums.scala")
  @Benchmark def implicitNumsBootstrapped() = compile(implicitNumsArgs)

  val implicitNumsTastyArgs = Array("-from-tasty", s"$implicitNumsOut/Test.tasty")
  @Benchmark def implicitNumsTasty() = compile(implicitNumsTastyArgs)

  val implicitScopeLoopArgs = Array("../tests/bench/implicit-scope-loop.scala")
  @Benchmark def implicitScopeLoopNightly() = compile(implicitScopeLoopArgs)

  val inductiveImplicitsArgs = Array("../tests/bench/inductive-implicits.scala")
  @Measurement(iterations = 50)
  @Benchmark
  def inductiveImplicitsNightly() = compile(inductiveImplicitsArgs)

  val findRefArgs = Array("../tests/bench/FindRef.scala")
  @Benchmark def findRef() = compile(findRefArgs)

  val patmatExhaustArgs = Array("-Wconf:any:s", "../tests/bench/patmatexhaust.scala")
  @Benchmark def patmatExhaust() = compile(patmatExhaustArgs)

  val exhaustivityIArgs = Array("../tests/bench/exhaustivity-I.scala")
  @Benchmark def exhaustivityINightly() = compile(exhaustivityIArgs)

  val exhaustivitySArgs = Array("-Wconf:any:s", "../tests/bench/exhaustivity-S.scala")
  @Benchmark def exhaustivitySNightly() = compile(exhaustivitySArgs)

  val exhaustivityTArgs = Array("../tests/bench/exhaustivity-T.scala")
  @Benchmark def exhaustivityTNightly() = compile(exhaustivityTArgs)

  val exhaustivityVArgs = Array("-Wconf:any:s", "../tests/bench/exhaustivity-V.scala")
  @Benchmark def exhaustivityVNightly() = compile(exhaustivityVArgs)

  val patmatI7186Args = Array("../tests/patmat/i7186.scala")
  @Benchmark def patmatI7186Nightly() = compile(patmatI7186Args)

  val patmatI12241Args = Array("-Wconf:any:s", "../tests/patmat/i12241.scala")
  @Benchmark def patmatI12241Nightly() = compile(patmatI12241Args)

  val patmatI12358Args = Array("-Wconf:any:s", "../tests/patmat/i12358.scala")
  @Benchmark def patmatI12358Nightly() = compile(patmatI12358Args)

  val posI13565Args = Array("../tests/pos/i13565.scala")
  @Benchmark def posI13565Nightly() = compile(posI13565Args)

  val i1535Args = Array("../tests/bench/i1535.scala")
  @Benchmark def i1535Nightly() = compile(i1535Args)

  val i1687Args = Array("../tests/bench/i1687.scala")
  @Benchmark def i1687Nightly() = compile(i1687Args)

  val emptyObjectArgs = Array("../tests/bench/empty-object.scala")
  @Benchmark def emptyObject() = compile(emptyObjectArgs)

  val emptyClassArgs = Array("../tests/bench/empty-class.scala")
  @Benchmark def emptyClassNightly() = compile(emptyClassArgs)

  val emptyFileArgs = Array("../tests/bench/empty-file.scala")
  @Benchmark def emptyFileNightly() = compile(emptyFileArgs)

  val powerMacroSource = "../tests/bench/power-macro/PowerMacro.scala"
  val powerMacroOut = s"$OUT_DIR/powerMacro"
  val powerMacro1Args = Array(
    "-classpath", s"$powerMacroOut:$benchClassPath",
    "../tests/bench/power-macro/PowerInlined-1.scala"
  )
  @Benchmark def powerMacro1Nightly() = compile(powerMacro1Args)

  val powerMacro1kArgs = Array(
    "-classpath", s"$powerMacroOut:$benchClassPath",
    "../tests/bench/power-macro/PowerInlined-1k.scala"
  )
  @Benchmark def powerMacro1kNightly() = compile(powerMacro1kArgs)

  val stringInterpolationMacroSource = "../tests/bench/string-interpolation-macro/Macro.scala"
  val stringInterpolationMacroOut = s"$OUT_DIR/string-interpolation-macro"
  val stringInterpolationMacroArgs = Array(
    "-classpath", s"$stringInterpolationMacroOut:$benchClassPath",
    "../tests/bench/string-interpolation-macro/Test.scala"
  )
  @Benchmark def stringInterpolationMacroNightly() = compile(stringInterpolationMacroArgs)

  val tuple22CreationApplyArgs = Array("../tests/bench/tuple22-creation-apply.scala")
  @Benchmark def tuple22CreationApplyNightly() = compile(tuple22CreationApplyArgs)

  val tuple22CreationConsArgs = Array("../tests/bench/tuple22-creation-cons.scala")
  @Benchmark def tuple22CreationConsNightly() = compile(tuple22CreationConsArgs)

  val tuple22TailsArgs = Array("../tests/bench/tuple22-tails.scala")
  @Benchmark def tuple22TailsNightly() = compile(tuple22TailsArgs)

  val tuple22ApplyArgs = Array("../tests/bench/tuple22-apply.scala")
  @Benchmark def tuple22ApplyNightly() = compile(tuple22ApplyArgs)

  val tuple22SizeArgs = Array("../tests/bench/tuple22-size.scala")
  @Benchmark def tuple22SizeNightly() = compile(tuple22SizeArgs)

  val compiletimeSumConstantsArgs = Array("tests-generated/compiletime-ops/sum-constants.scala")
  @Benchmark def compiletimeSumConstantsNightly() = compile(compiletimeSumConstantsArgs)

  val compiletimeSumTermrefsArgs = Array("tests-generated/compiletime-ops/sum-termrefs.scala")
  @Benchmark def compiletimeSumTermrefsNightly() = compile(compiletimeSumTermrefsArgs)

  val compiletimeSumTermsArgs = Array("tests-generated/compiletime-ops/sum-terms.scala")
  @Benchmark def compiletimeSumTermsNightly() = compile(compiletimeSumTermsArgs)

  val compiletimeSumApplicationsArgs = Array("tests-generated/compiletime-ops/sum-applications.scala")
  @Benchmark def compiletimeSumApplicationsNightly() = compile(compiletimeSumApplicationsArgs)

  val compiletimeDistributeArgs = Array("tests-generated/compiletime-ops/distribute.scala")
  @Benchmark def compiletimeDistributeNightly() = compile(compiletimeDistributeArgs)
