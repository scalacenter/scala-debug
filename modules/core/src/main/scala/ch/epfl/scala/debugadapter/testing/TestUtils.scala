package ch.epfl.scala.debugadapter.testing

import sbt.testing.*

import java.io.ByteArrayOutputStream
import java.io.PrintStream

object TestUtils {
  def printSelector(selector: Selector): Option[String] = selector match {
    case c: TestSelector => Some(c.testName())
    case c: SuiteSelector => Some(c.toString)
    case c: NestedSuiteSelector => Some(c.suiteId())
    case c: TestWildcardSelector => Some(c.testWildcard())
    case c: NestedTestSelector => Some(c.testName())
    case _ => None
  }

  def printThrowable(opt: OptionalThrowable): Option[String] = {
    if (opt.isEmpty) None
    else Some(stripTestFrameworkSpecificInformation(opt.get().getMessage))
  }

  def printStackTrace(opt: OptionalThrowable): Option[String] = {
    if (opt.isEmpty) None
    else
      Some {
        val writer = new StringBuffer
        val outputStream = new ByteArrayOutputStream()
        val printStream = new PrintStream(outputStream)
        opt.get().printStackTrace(printStream)
        outputStream.toString
      }
  }

  private val specs2Prefix = "java.lang.Exception: "
  private val utestPrefix = "utest.AssertionError: "
  private val scalaTestPrefix = "org.scalatest.exceptions.TestFailedException: "

  def stripTestFrameworkSpecificInformation(message: String): String =
    if (message.startsWith(scalaTestPrefix))
      message.drop(scalaTestPrefix.length)
    else if (message.startsWith(specs2Prefix)) message.drop(specs2Prefix.length)
    else if (message.startsWith(utestPrefix)) message.drop(utestPrefix.length)
    else message
}
