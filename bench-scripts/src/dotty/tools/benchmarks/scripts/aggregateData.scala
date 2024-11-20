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
    q3: Double,
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
@main def aggregateData(dataDirString: String): Unit =
  given CSVFormat = new DefaultCSVFormat:
    override val lineTerminator = "\n"

  val dataDir = parsePath(dataDirString)
  assert(os.exists(dataDir), s"`$dataDir` not found.")

  val rawData = dataDir / "raw"
  val aggregatedData = dataDir / "aggregated"

  os.remove.all(aggregatedData)
  os.makeDir.all(aggregatedData / "all")
  os.makeDir.all(aggregatedData / "last100")

  val rows =
    for file <- os.walk(rawData) if file.ext == "csv" yield
      val reader = CSVReader.open(file.toString())
      val fileRows =
        for row <- reader.all() if row.nonEmpty yield
          val res = Results.fromCSVRow(row)
          val sorted = res.measures.sorted
          assert(sorted.nonEmpty, s"Empty measures for benchmark `${res.benchmark}`.")
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
      fileRows

  for (benchmark, benchmarkRows) <- rows.flatten.groupBy(_.benchmark) do
    writeAggregatedRows(benchmarkRows, aggregatedData / "all" / s"$benchmark.csv")
    writeAggregatedRows(benchmarkRows.takeRight(100), aggregatedData / "last100" / s"$benchmark.csv")

  println(s"Wrote ${rows.length} aggregated rows to `aggregatedData`.")

/** Writes a sequence of [[AggregatedRow]]s in CSV at the given `path`. */
def writeAggregatedRows(rows: collection.Seq[AggregatedRow], path: os.Path) =
  val writer = CSVWriter.open(path.toString())
  for row <- rows do
    writer.writeRow(Seq(row.commitTime.toString(), row.commit, row.pr, row.min, row.q1, row.median, row.q3, row.max))
  writer.close()
