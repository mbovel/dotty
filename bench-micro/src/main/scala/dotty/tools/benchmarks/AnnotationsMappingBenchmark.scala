package dotty.tools.benchmarks

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Level, Measurement, Mode => JMHMode, Param, Scope, Setup, State, Warmup}
import java.util.concurrent.TimeUnit.SECONDS

import dotty.tools.dotc.{Driver, Run, Compiler}
import dotty.tools.dotc.ast.{tpd, TreeTypeMap}, tpd.{Tree}
import dotty.tools.dotc.core.Annotations.{ConcreteAnnotation, Annotation}
import dotty.tools.dotc.core.Contexts.{ContextBase, Context, ctx, withMode}
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Symbols.{defn, mapSymbols, Symbol}
import dotty.tools.dotc.core.Types.{AnnotatedType, SkolemType, TermRef, Type, TypeMap}
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.typer.TyperPhase

@Fork(value = 4)
@Warmup(iterations = 1, time = 2, timeUnit = SECONDS)
@Measurement(iterations = 2, time = 2, timeUnit = SECONDS)
@BenchmarkMode(Array(JMHMode.Throughput))
@State(Scope.Thread)
class AnnotationsMappingBenchmark:
  var tp: Type = null
  var typeMap: TypeMap = null
  var typeFunction: Context ?=> Type => Type = null
  var context: Context = null
  var specialIntTp: Type = null

  @Param(Array("v1", "v2", "v3", "v4"))
  var valName: String = null

  @Param(Array("current", "traverseTrees", "traverseTreesCopySymbols"))
  var typeMapName: String = null

  @Param(Array("id", "mapInts"))
  var typeFunctionName: String = null

  @Setup(Level.Iteration)
  def setup(): Unit =
    val testPhase =
      new Phase:
        final override def phaseName = "testPhase"
        final override def run(using ctx: Context): Unit =
          val pkg = ctx.compilationUnit.tpdTree.symbol
          tp = pkg.requiredClass("Test").requiredValueRef(valName).underlying
          specialIntTp = pkg.requiredClass("Test").requiredType("SpecialInt").typeRef
          context = ctx

    val compiler =
      new Compiler:
        private final val baseCompiler = new Compiler()
        final override def phases = List(List(Parser()), List(TyperPhase()), List(testPhase))

    val driver =
      new Driver:
        final override def newCompiler(using Context): Compiler = compiler

    val classPath = System.getProperty("BENCH_CLASS_PATH")
    driver.process(Array("-classpath", classPath, "tests/someAnnotatedTypes.scala"))

    typeMap =
      typeMapName match
        case "current" =>
          new TypeMap(using context):
            final override def apply(tp: Type): Type = typeFunction(mapOver(tp))
        case "traverseTrees" =>
          new TypeMap(using context):
            final override def apply(tp: Type): Type = typeFunction(mapOver(tp))
            final override def mapOver(tp: Type) =
              tp match
                case tp @ AnnotatedType(underlying, annot) =>
                  val underlying1 = this(underlying)
                  val annot1 =
                    annot match
                      case ConcreteAnnotation(t: Tree) => annot.derivedAnnotation(this.mapOver(t))
                      case _ => annot.mapWith(this)
                  derivedAnnotatedType(tp, underlying1, annot1)
                case _ => super.mapOver(tp)
        case "traverseTreesCopySymbols" =>
          new TypeMap(using context):
            final override def apply(tp: Type): Type = typeFunction(mapOver(tp))
            final override def mapOver(tp: Type) =
              tp match
                case tp @ AnnotatedType(underlying, annot) =>
                  val underlying1 = this(underlying)
                  val annot1 =
                    annot match
                      case ConcreteAnnotation(t: Tree) =>
                        val ttm =
                          new TreeTypeMap(this):
                            final override def withMappedSyms(syms: List[Symbol]): TreeTypeMap =
                              withMappedSyms(syms, mapSymbols(syms, this, mapAlways = true))
                        annot.derivedAnnotation(ttm.transform(t))
                      case _ => annot.mapWith(this)
                  derivedAnnotatedType(tp, underlying1, annot1)
                case _ => super.mapOver(tp)
        case _ => throw new IllegalArgumentException(s"Unknown type map: $typeMapName")

    typeFunction =
      typeFunctionName match
        case "id" => tp => tp
        case "mapInts" => tp => (if tp frozen_=:= defn.IntType then specialIntTp else tp)
        case _ => throw new IllegalArgumentException(s"Unknown type function: $typeFunctionName")

  @Benchmark
  def applyTypeMap() =
    val res = typeMap.apply(tp)
    //println(res.show(using context))
