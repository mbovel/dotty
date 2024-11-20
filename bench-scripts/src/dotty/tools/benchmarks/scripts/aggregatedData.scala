package dotty.tools.benchmarks.scripts

import java.time.ZonedDateTime
import com.github.tototoshi.csv.{CSVReader, CSVWriter, CSVFormat, DefaultCSVFormat}

/** Aggregated results for one run of a benchmark at a specific commit. */
case class AggregatedRow(
    benchmark: String,
    commitTime: ZonedDateTime,
    commit: String,
    pr: Int,
    min: Double,
    q1: Double,
    median: Double,
    q2: Double,
    max: Double
)

/** Computes aggregated data from benchmark results in `dataCsv` and writes it
  * to the `output` directory.
  *
  * The input CSV file is expected to contain [[Results]] rows: one row per
  * benchmark result.
  *
  * **The output directory is cleared before writing the aggregated data.**
  *
  * Afterwards, two CSV files are written for each benchmark:
  *   - `all/[benchmark].csv`: all aggregated rows,
  *   - `last100/[benchmark].csv`: the last 100 aggregated rows.
  */
@main def aggregatedData(dataCsv: String, output: String): Unit =
  given CSVFormat = new DefaultCSVFormat:
    override val lineTerminator = "\n"

  val dataCsvPath = parsePath(dataCsv)
  val outputPath = parsePath(output)

  assert(os.exists(dataCsvPath), s"`$dataCsvPath` not found.")
  assert(os.exists(outputPath), s"`$outputPath` not found.")

  os.remove.all(outputPath)
  os.makeDir.all(outputPath / "all")
  os.makeDir.all(outputPath / "last100")

  val reader = CSVReader.open(dataCsvPath.toString())
  val rows =
    for row <- reader.iterator yield
      val res = Results.fromCSVRow(row)
      val sorted = res.measures.sorted
      AggregatedRow(
        res.benchmark,
        res.commitTime,
        res.commit,
        res.pr,
        sorted.head,
        sorted.percentile(0.25),
        sorted.percentile(0.5),
        sorted.percentile(0.75),
        sorted.last
      )
  reader.close()

  for (benchmark, benchmarkRows) <- rows.toSeq.groupBy(_.benchmark) do
    writeAggregatedRows(benchmarkRows, outputPath / "all" / s"$benchmark.csv")
    writeAggregatedRows(benchmarkRows.takeRight(100), outputPath / "last100" / s"$benchmark.csv")

  println(s"Wrote ${rows.length} aggregated rows to `$outputPath/aggregated`.")

/** Writes a sequence of [[AggregatedRow]]s in CSV at the given `path`. */
def writeAggregatedRows(rows: collection.Seq[AggregatedRow], path: os.Path) =
  val writer = CSVWriter.open(path.toString())
  for row <- rows do
    writer.writeRow(Seq(row.commitTime.toString(), row.commit, row.pr, row.min, row.median, row.max))
  writer.close()
