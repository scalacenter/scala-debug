package ch.epfl.scala.debugadapter.testing

import sbt.testing.Status
import scala.jdk.CollectionConverters.*

trait TestSuiteEventHandler {
  def handle(testSuiteEvent: TestSuiteEvent): Unit
}

object TestSuiteEventHandler {
  def formatError(
      testName: String,
      failureMessage: String,
      indentSize: Int
  ): String = {
    val indent = " " * indentSize
    (testName, failureMessage) match {
      case ("", failureMessage) => s"$indent* $failureMessage"
      case (testName, "") => s"$indent* $testName"
      case (testName, failureMessage) => s"$indent* $testName - $failureMessage"
    }
  }

  /**
   * Provide a summary of test suite execution based on passed TestSuiteEvent.Results parameter.
   */
  def summarizeResults(
      testSuiteResult: TestSuiteEvent.Results
  ): TestSuiteSummary = {
    val results = testSuiteResult.events.map { e =>
      val name = TestUtils.printSelector(e.selector).getOrElse("")
      val testResult: SingleTestSummary = e.status() match {
        case Status.Success =>
          SingleTestResult.Passed(name, e.duration)
        case Status.Failure =>
          val failedMsg =
            TestUtils.printThrowable(e.throwable()).getOrElse("")
          val stackTrace = TestUtils.printStackTrace(e.throwable()).getOrElse("")
          val formatted =
            TestSuiteEventHandler.formatError(
              name,
              failedMsg,
              indentSize = 0
            )
          SingleTestResult.Failed(name, e.duration, formatted, stackTrace)
        case _ =>
          SingleTestResult.Skipped(name)
      }
      testResult
    }.asJava

    TestSuiteSummary(
      suiteName = testSuiteResult.testSuite,
      duration = testSuiteResult.duration,
      tests = results
    )
  }
}
