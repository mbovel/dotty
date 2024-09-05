package dotty.tools.benchmarks.scripts

import org.junit.{Test, Assert}
import Assert.assertEquals

class ImportResultsTests:

  @Test def parseBenchmarkNameSimple() =
    assertEquals(
      "parseBenchmarkName should return the last part of the method name",
      "implicitCacheBootstrapped",
      parseBenchmarkName("dotty.tools.benchmarks.CompilationBenchmarks.implicitCacheBootstrapped")
    )

  @Test def parseBenchmarkNameNightly() =
    assertEquals(
      "parseBenchmarkName should remove the 'Nightly' suffix",
      "dotty",
      parseBenchmarkName("dotty.tools.benchmarks.CompilationBenchmarks.dottyNightly")
    )

  @Test def readResultsExample1() =
    assertEquals(
      List(
        JMHResults("implicitCacheBootstrapped", List(562.817, 386.297, 365.383), List(366.542, 390.866, 352.338)),
        JMHResults("implicitCacheTasty", List(899.944, 687.595, 625.322), List(616.428, 551.615, 552.089)),
        JMHResults("implicitNumsBootstrapped", List(1934.992, 1389.856, 1440.386), List(1731.511, 1083.199, 886.713)),
        JMHResults("implicitNumsTasty", List(986.655, 755.808, 695.625), List(599.294, 566.847, 468.301)),
        JMHResults("implicitScopeLoop", List(397.422, 398.656, 333.208), List(332.119, 328.872, 339.061))
      ),
      readJMHResults(os.resource / "example_jmh_output_1.txt")
    )

  @Test def importResultsExample1() =
    val jmhOutput = os.resource / "example_jmh_output_1.txt"
    val outputFile = os.temp()
    val stdout = java.io.ByteArrayOutputStream()

    Console.withOut(stdout) {
      importResults(
        2,
        "abc123",
        true,
        0,
        jmhOutput,
        outputFile,
        parseDate("2021-09-01T00:00:01+01:00"),
        parseDate("2021-09-01T00:05-02:00"),
      )
    }

    assertEquals(
      """
      implicitCacheBootstrapped,2021-08-31T23:00:01Z,abc123,true,2,2021-09-01T02:05:00Z,0,562.817 386.297 365.383,366.542 390.866 352.338
      implicitCacheTasty,2021-08-31T23:00:01Z,abc123,true,2,2021-09-01T02:05:00Z,0,899.944 687.595 625.322,616.428 551.615 552.089
      implicitNumsBootstrapped,2021-08-31T23:00:01Z,abc123,true,2,2021-09-01T02:05:00Z,0,1934.992 1389.856 1440.386,1731.511 1083.199 886.713
      implicitNumsTasty,2021-08-31T23:00:01Z,abc123,true,2,2021-09-01T02:05:00Z,0,986.655 755.808 695.625,599.294 566.847 468.301
      implicitScopeLoop,2021-08-31T23:00:01Z,abc123,true,2,2021-09-01T02:05:00Z,0,397.422 398.656 333.208,332.119 328.872 339.061
      """.stripIndent().stripLeading(), // trailing "\n" expected
      os.read(outputFile)
    )

    assert(stdout.toString().contains("Write results for benchmark `implicitScopeLoop`"))
