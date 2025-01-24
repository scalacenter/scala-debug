package ch.epfl.scala.debugadapter.testing

/**
 * Summary of test suite execution which is being send to the dap client.
 * Because of gson serialization, this class uses Java List rather than a scala collection.
 */
final case class TestSuiteSummary(
    suiteName: String,
    duration: Long,
    tests: java.util.List[SingleTestSummary]
)

/**
 * Sealed hierarchy that models 3 possible outcomes of single test case.
 * I wanted to model this a discriminated union type and due to gson serialization,
 * the best solution I was able to find at that moment was additional kind field.
 * Each class has a smart constructor available in the companion objects which sets value of kind correctly.
 */
sealed trait SingleTestSummary
object SingleTestResult {
  final class Passed private (
      val kind: String,
      val testName: String,
      val duration: Long
  ) extends SingleTestSummary
  object Passed {
    def apply(testName: String, duration: Long): Passed =
      new Passed("passed", testName, duration)
  }

  final class Skipped private (
      val kind: String,
      val testName: String
  ) extends SingleTestSummary
  object Skipped {
    def apply(testName: String): Skipped = new Skipped("skipped", testName)
  }

  final class Failed private (
      val kind: String,
      val testName: String,
      val duration: Long,
      val error: String,
      val stackTrace: String
  ) extends SingleTestSummary
  object Failed {
    def apply(testName: String, duration: Long, error: String, stackTrace: String): Failed =
      new Failed("failed", testName, duration, error, stackTrace)
  }
}
