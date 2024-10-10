import org.openjdk.jmh.profile.InternalProfiler
import org.openjdk.jmh.infra.{BenchmarkParams, IterationParams}
import org.openjdk.jmh.results.{Result, ScalarResult, IterationResult}
import org.openjdk.jmh.results.AggregationPolicy

import scala.jdk.CollectionConverters.*

import java.util.Collection

import dotty.tools.dotc.util.Stats

class DottyStatsProfiler extends InternalProfiler {
  def beforeIteration(benchmarkParams: BenchmarkParams, iterationParams: IterationParams): Unit =
    Stats.hits.clear()

  def afterIteration(benchmarkParams: BenchmarkParams, iterationParams: IterationParams, iterationResult: IterationResult): Collection[Result[ScalarResult]] =
    Stats.hits
         .iterator
         .map((key, value) =>
            ScalarResult(
              key
                .replaceAll("\n", " ")
                .replaceAll("\\{", "(")
                .replaceAll("}", ")")
                .replaceAll(",", " "),
              value.toDouble,
              if key.endsWith(" μs") then "μs" else "counts",
              AggregationPolicy.AVG
            )
         )
         .toList
         .asJavaCollection

  def getDescription(): String = "Dotty Stats Profiler"
}
