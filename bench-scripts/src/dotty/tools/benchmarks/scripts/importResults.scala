package dotty.tools.benchmarks.scripts

import java.time.{ZonedDateTime, ZoneOffset}
import collection.mutable.ArrayBuffer
import com.github.tototoshi.csv.{CSVWriter, CSVFormat, DefaultCSVFormat}

/** Results for one run of a benchmark, as imported from a JMH output file. */
case class JMHResults(benchmark: String, warmup: Seq[Double], measures: Seq[Double])

/** Imports benchmark results from a JMH output file into a CSV file. */
@main def importResults(
    prString: String,
    commitTimeString: String,
    commit: String,
    benchTimeString: String,
    jmhOutput: String,
    dataCsv: String
): Unit =
  given CSVFormat = new DefaultCSVFormat:
    override val lineTerminator = "\n"

  println("Importing benchmark results...")

  val pr = prString.toInt
  val commitTime = parseDate(commitTimeString)
  val benchTime = parseDate(benchTimeString)
  val jmhOutputPath = parsePath(jmhOutput)
  val dataCsvPath = parsePath(dataCsv)

  println(s"pwd: ${os.pwd}")
  println(s"pr: $pr")
  println(s"commitTime: $commitTime")
  println(s"commit: $commit")
  println(s"benchTime: $benchTime")
  println(s"jmhOutputPath: $jmhOutputPath")
  println(s"dataCsvPath: $dataCsvPath")

  assert(os.exists(jmhOutputPath), s"`$jmhOutputPath` not found.")
  assert(os.exists(dataCsvPath), s"`$dataCsvPath` not found.")

  val writer = CSVWriter.open(dataCsvPath.toString(), append = true)

  for jmhResults <- readJMHResults(jmhOutputPath) do
    println(s"Write results for benchmark `${jmhResults.benchmark}`")
    val resultsRow = Results(
      jmhResults.benchmark,
      commitTime,
      commit,
      pr,
      benchTime,
      jmhResults.warmup,
      jmhResults.measures
    ).toCSVRow()
    writer.writeRow(resultsRow)
  writer.close()

/** Reads results from a JMH text output file. */
def readJMHResults(jmhOutputPath: os.ReadablePath): Seq[JMHResults] =
  val benchmarkPrefix = "# Benchmark: "
  val warmupPrefix = "# Warmup Iteration"
  val measurePrefix = "Iteration "
  val lines = os.read.lines(jmhOutputPath)
  val results = ArrayBuffer.empty[JMHResults]
  var benchmark = ""
  val warmup = ArrayBuffer.empty[Double]
  val measures = ArrayBuffer.empty[Double]
  for line <- lines do
    if line.startsWith(benchmarkPrefix) then
      if benchmark.nonEmpty then
        results += JMHResults(benchmark, warmup.toSeq, measures.toSeq)
        warmup.clear()
        measures.clear()
      benchmark = parseBenchmarkName(readValue(line))
    if line.startsWith(warmupPrefix) then
      warmup += parseTime(readValue(line))
    if line.startsWith(measurePrefix) then
      measures += parseTime(readValue(line))
  results += JMHResults(benchmark, warmup.toSeq, measures.toSeq)
  results.toSeq

/** Reads the value of a line that has the format `key: value`. */
def readValue(line: String): String =
  val parts = line.split(":")
  assert(parts.length == 2, s"expected 2 parts separated by ':' in line '$line'")
  parts(1).trim

/** Parses a benchmark method name into a short name. */
def parseBenchmarkName(methodName: String): String =
  val nightlySuffix = "Nightly"
  val name = methodName.split("\\.").last
  if name.endsWith(nightlySuffix) then name.dropRight(nightlySuffix.length) else name

/** Parses a time value from a JMH output line. It must end with 'ms/op'. */
def parseTime(time: String): Double =
  val timeUnit = " ms/op"
  assert(time.endsWith(timeUnit), s"expected $time to end with time unit '$timeUnit'")
  time.dropRight(timeUnit.length).toDouble

/** Parses a date string in ISO format to a [[ZonedDateTime]] in UTC. */
def parseDate(s: String): ZonedDateTime =
  ZonedDateTime.parse(s).withZoneSameInstant(ZoneOffset.UTC).withNano(0)

/** Parses a path string to an [[os.Path]].
  *
  * If the path is relative, it is resolved against the current working
  * directory.
  */
def parsePath(s: String): os.Path =
  os.Path(s, os.pwd)
