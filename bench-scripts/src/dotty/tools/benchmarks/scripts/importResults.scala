package dotty.tools.benchmarks.scripts

import scala.sys.process.stringToProcess
import java.time.{ZonedDateTime, ZoneOffset}
import collection.mutable.ArrayBuffer
import com.github.tototoshi.csv.{CSVWriter, CSVFormat, DefaultCSVFormat}

/** Results for one run of a benchmark, as imported from a JMH output file. */
case class JMHResults(benchmark: String, warmup: Seq[Double], measures: Seq[Double])

/** Imports benchmark results from a JMH output file into a CSV file. */
@main def importResults(
    pr: Int,
    commit: String,
    merged: Boolean,
    run: Int,
    jmhOutputString: String,
    dataCsvString: String
): Unit =
  val jmhOutput = parsePath(jmhOutputString)
  assert(os.exists(jmhOutput), s"`$jmhOutput` not found.")
  val dataCsv = parsePath(dataCsvString)
  assert(!os.exists(dataCsv), s"`$dataCsv` already exists.")
  val actualCommit = "git rev-parse HEAD".!!.trim
  assert(commit == actualCommit, "commit does not match HEAD")
  val commitTime = parseDate("git show -s --format=%cI".!!.trim)
  val benchTime = ZonedDateTime.now().withZoneSameInstant(ZoneOffset.UTC).withNano(0)
  importResults(pr, commit, merged, run, jmhOutput, dataCsv, commitTime, benchTime)

def importResults(
    pr: Int,
    commit: String,
    merged: Boolean,
    run: Int,
    jmhOutput: os.ReadablePath,
    dataCsv: os.Path,
    commitTime: ZonedDateTime,
    benchTime: ZonedDateTime
): Unit =
  println("Importing benchmark results...")
  println(s"pwd: ${os.pwd}")
  println(s"pr: $pr")
  println(s"commitTime: $commitTime")
  println(s"commit: $commit")
  println(s"benchTime: $benchTime")
  println(s"jmhOutput: $jmhOutput")
  println(s"dataCsv: $dataCsv")
  os.makeDir.all(dataCsv / os.up)

  given CSVFormat = new DefaultCSVFormat:
    override val lineTerminator = "\n"

  val writer = CSVWriter.open(dataCsv.toString(), append = true)
  for jmhResults <- readJMHResults(jmhOutput) do
    println(s"Write results for benchmark `${jmhResults.benchmark}`")
    val resultsRow = Results(
      jmhResults.benchmark,
      commitTime,
      commit,
      merged,
      pr,
      benchTime,
      run,
      jmhResults.warmup,
      jmhResults.measures
    ).toCSVRow()
    writer.writeRow(resultsRow)
  writer.close()
  println(s"Wrote results to $dataCsv.")

/** Reads results from a JMH text output file. */
def readJMHResults(jmhOutput: os.ReadablePath): Seq[JMHResults] =
  val benchmarkPrefix = "# Benchmark: "
  val warmupPrefix = "# Warmup Iteration"
  val measurePrefix = "Iteration "
  val lines = os.read.lines(jmhOutput)
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
