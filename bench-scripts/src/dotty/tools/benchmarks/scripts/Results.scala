package dotty.tools.benchmarks.scripts

import java.time.{ZonedDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter

/** Raw results for one run of a benchmark at a specific commit.
  *
  * @param benchmark
  *   Name of the benchmark
  * @param commitTime
  *   Time of the benchmarked commit
  * @param commit
  *   Hash of the benchmarked commit
  * @param pr
  *   Pull request corresponding to the benchmarked commit
  * @param benchTime
  *   Time at which the benchmark was run
  * @param warmup
  *   Warmup times
  * @param measures
  *   Measurement times
  */
case class Results(
    benchmark: String,
    commitTime: ZonedDateTime,
    commit: String,
    pr: Int,
    benchTime: ZonedDateTime,
    warmup: Seq[Double],
    measures: Seq[Double]
):
  assert(commitTime.getZone() == ZoneOffset.UTC, s"expected commit time '$commitTime' to be in UTC")
  assert(benchTime.getZone() == ZoneOffset.UTC, s"expected benchmark time '$benchTime' to be in UTC")
  assert(benchTime.isAfter(commitTime), s"expected benchmark time '$benchTime' to be after commit time '$commitTime'")

  /** Converts this result to a CSV row. */
  def toCSVRow(): Seq[String] = Seq(
    benchmark,
    commitTime.format(DateTimeFormatter.ISO_DATE_TIME),
    commit,
    pr.toString,
    benchTime.format(DateTimeFormatter.ISO_DATE_TIME),
    warmup.mkString(" "),
    measures.mkString(" ")
  )

object Results:
  /** Reads a [[Result]] from a CSV row. */
  def fromCSVRow(row: Seq[String]): Results =
    val Seq(benchmark, pr, merged, commitTime, commit, benchTime, warmupRaw, measuresRaw) = row
    Results(
      benchmark,
      ZonedDateTime.parse(commitTime),
      commit,
      pr.toInt,
      ZonedDateTime.parse(benchTime),
      warmupRaw.trim().split(" ").filter(_.nonEmpty).toSeq.map(_.toDouble),
      measuresRaw.trim().split(" ").filter(_.nonEmpty).toSeq.map(_.toDouble)
    )
