package ch.epfl.scala.debugadapter.testfmk

import com.microsoft.java.debug.core.protocol.Types.StackFrame
import munit.Assertions.*
import munit.Location
import com.microsoft.java.debug.core.protocol.Types.Variable

import java.nio.file.Path
import scala.Console.*

import DebugStepAssert.*

sealed trait DebugStepAssert

object DebugStepAssert {
  def inParallel(steps: SingleStepAssert[Either[String, String]]*): ParallelStepsAsserts[Either[String, String]] =
    ParallelStepsAsserts(steps)

  def assertOnFrame(expectedSource: Path, expectedLine: Int)(callStack: Array[StackFrame])(implicit
      location: Location
  ): Unit = {
    assert(callStack != null)
    assertEquals(callStack.head.source.path, expectedSource.toString)
    assertEquals(callStack.head.line, expectedLine)
  }

  def assertOnFrame(expectedName: String)(frames: Array[StackFrame])(implicit loc: Location): Unit = {
    assert(frames != null)
    assertEquals(frames.head.name, expectedName)
  }

  def assertCallStack(expected: Seq[String])(callStack: Array[StackFrame])(implicit
      location: Location
  ): Unit = {
    assert(callStack != null)
    val obtained = callStack.collect { case frame if frame.presentationHint != "subtle" => frame.name }.toSeq
    assertEquals(obtained, expected)
  }
}

final case class SingleStepAssert[T](step: DebugStep[T], assertion: T => Unit) extends DebugStepAssert
final case class ParallelStepsAsserts[T](steps: Seq[SingleStepAssert[T]]) extends DebugStepAssert

sealed trait DebugStep[+T]

final case class Breakpoint(sourceFile: Path, line: Int, condition: Option[String]) extends DebugStep[Array[StackFrame]]
object Breakpoint {
  def apply(line: Int)(implicit ctx: TestingContext, location: Location): SingleStepAssert[Array[StackFrame]] = {
    val breakpoint = Breakpoint(ctx.mainSource, line, None)
    SingleStepAssert(breakpoint, assertOnFrame(ctx.mainSource, line))
  }

  def apply(sourceFile: Path, line: Int)(implicit location: Location): SingleStepAssert[Array[StackFrame]] = {
    val breakpoint = Breakpoint(sourceFile, line, None)
    SingleStepAssert(breakpoint, assertOnFrame(sourceFile, line))
  }

  def apply(sourceFile: Path, line: Int, expectedTopFrame: String)(implicit
      location: Location
  ): SingleStepAssert[Array[StackFrame]] = {
    val breakpoint = Breakpoint(sourceFile, line, None)
    SingleStepAssert(
      breakpoint,
      { callStack =>
        assertOnFrame(sourceFile, line)(callStack); assertOnFrame(expectedTopFrame)(callStack)
      }
    )
  }

  def apply(line: Int, expectedCallStack: Seq[String])(implicit
      ctx: TestingContext,
      location: Location
  ): SingleStepAssert[Array[StackFrame]] = {
    val breakpoint = Breakpoint(ctx.mainSource, line, None)
    SingleStepAssert(
      breakpoint,
      { trace =>
        assertOnFrame(ctx.mainSource, line)(trace); assertCallStack(expectedCallStack)(trace)
      }
    )
  }

  def apply(line: Int, condition: String)(implicit ctx: TestingContext): SingleStepAssert[Array[StackFrame]] = {
    val breakpoint = Breakpoint(ctx.mainSource, line, Some(condition))
    SingleStepAssert(breakpoint, assertOnFrame(ctx.mainSource, line))
  }
}

final case class Logpoint(sourceFile: Path, line: Int, logMessage: String) extends DebugStep[String]
object Logpoint {
  def apply(line: Int, logMessage: String, expected: String)(implicit
      ctx: TestingContext,
      loc: Location
  ): SingleStepAssert[String] = {
    val logpoint = Logpoint(ctx.mainSource, line, logMessage)
    SingleStepAssert(logpoint, output => assertEquals(output, expected))
  }
}

object StepIn extends DebugStep[Array[StackFrame]] {
  def line(line: Int)(implicit ctx: TestingContext, location: Location): SingleStepAssert[Array[StackFrame]] =
    SingleStepAssert(StepIn, assertOnFrame(ctx.mainSource, line))

  def method(methodName: String)(implicit loc: Location): SingleStepAssert[Array[StackFrame]] =
    SingleStepAssert(StepIn, assertOnFrame(methodName))
}

object StepOut extends DebugStep[Array[StackFrame]] {
  def line(line: Int)(implicit ctx: TestingContext): SingleStepAssert[Array[StackFrame]] =
    SingleStepAssert(StepOut, assertOnFrame(ctx.mainSource, line))

  def method(methodName: String): SingleStepAssert[Array[StackFrame]] =
    SingleStepAssert(StepOut, assertOnFrame(methodName))
}

object StepOver extends DebugStep[Array[StackFrame]] {
  def line(line: Int)(implicit ctx: TestingContext): SingleStepAssert[Array[StackFrame]] =
    SingleStepAssert(StepOver, assertOnFrame(ctx.mainSource, line))

  def method(methodName: String): SingleStepAssert[Array[StackFrame]] =
    SingleStepAssert(StepOver, assertOnFrame(methodName))
}

final case class Evaluation(expression: String) extends DebugStep[Either[String, String]]
object Evaluation {
  def ignore(expression: String, expected: Any)(implicit
      ctx: TestingContext
  ): SingleStepAssert[Either[String, String]] =
    SingleStepAssert(Evaluation(expression), assertIgnore(expected.toString))

  def failed(expression: String, error: String)(implicit location: Location): SingleStepAssert[Either[String, String]] =
    SingleStepAssert(Evaluation(expression), assertFailed(error))

  def failedOrIgnore(expression: String, error: String, ignore: Boolean)(implicit
      ctx: TestingContext,
      location: Location
  ): SingleStepAssert[Either[String, String]] = {
    SingleStepAssert(
      Evaluation(expression),
      if (ignore) assertIgnore(error) _ else assertFailed(error) _
    )
  }

  def failed(expression: String)(
      assertion: String => Unit
  )(implicit ctx: TestingContext, location: Location) =
    SingleStepAssert(Evaluation(expression), assertFailed(assertion))

  def failedOrIgnore(expression: String, ignore: Boolean)(
      assertion: String => Unit
  )(implicit ctx: TestingContext, location: Location) = {
    SingleStepAssert(
      Evaluation(expression),
      if (ignore) assertIgnore("failure") _ else assertFailed(assertion) _
    )
  }

  def success(expression: String, result: Any)(implicit
      ctx: TestingContext,
      location: Location
  ): SingleStepAssert[Either[String, String]] =
    new SingleStepAssert(Evaluation(expression), assertSuccess(result))

  def success(
      expression: String
  )(assertion: String => Unit)(implicit location: Location): SingleStepAssert[Either[String, String]] = {
    new SingleStepAssert(Evaluation(expression), assertSuccess(assertion))
  }

  def successOrIgnore(expression: String, result: Any, ignore: Boolean)(implicit
      ctx: TestingContext,
      location: Location
  ): SingleStepAssert[Either[String, String]] = {
    val assertion = if (ignore) assertIgnore(result.toString) _ else assertSuccess(result)(_)
    new SingleStepAssert(Evaluation(expression), assertion)
  }

  def successOrIgnore(expression: String, ignore: Boolean)(
      assertion: String => Unit
  )(implicit ctx: TestingContext, location: Location): SingleStepAssert[Either[String, String]] = {
    new SingleStepAssert(
      Evaluation(expression),
      if (ignore) assertIgnore("sucess") _ else assertSuccess(assertion)(_)
    )
  }

  private def assertFailed(response: Either[String, String])(implicit location: Location): Unit =
    assert(response.isLeft, clue = s"Expected error, got ${response.toOption.get}$RESET")

  private def assertFailed(
      assertion: String => Unit
  )(response: Either[String, String])(implicit location: Location): Unit = {
    assertFailed(response)
    val error = response.left.toOption.get
    assertion(error)
  }

  private def assertFailed(expectedError: String)(response: Either[String, String])(implicit location: Location): Unit =
    assertFailed(error => assert(clue(error).contains(clue(expectedError))))(response)

  private def assertIgnore(
      expected: String
  )(response: Either[String, String])(implicit ctx: TestingContext): Unit = {
    println(s"${YELLOW}TODO fix in ${ctx.scalaVersion}: expected $expected$RESET")
  }

  private def assertSuccess(
      assertion: String => Unit
  )(response: Either[String, String])(implicit location: Location): Unit = {
    assert(clue(response).isRight)
    val result = response.toOption.get
    assertion(result)
  }

  private def assertSuccess(
      expectedResult: Any
  )(response: Either[String, String])(implicit ctx: TestingContext, location: Location): Unit = {
    if (clue(response).isLeft) println(s"${RED}Expected success, got ${response.left}$RESET")
    assert(clue(response).isRight)
    val result = response.toOption.get
    expectedResult match {
      case null => assertEquals(result, "null")
      case ObjectRef(clsName) =>
        assert(result.startsWith(clsName + "@"))
      case expected: String =>
        assertEquals(result, '"'.toString + expected + '"')
      case () =>
        assert(result.endsWith('"'.toString + "()" + '"') || result == "<void value>")
      case expected @ (_: Boolean | _: Byte | _: Char | _: Int | _: Long | _: Short) =>
        assertEquals(result, expected.toString)
      case floating @ (_: Double | _: Float) =>
        val expected = String.format("%f", floating.toString.toDouble: java.lang.Double)
        assertEquals(result, expected)
      case expected =>
        // they have the same toString
        assert(result.endsWith("\"" + expected + "\""))
    }
  }
}

final case class LocalVariable(names: Seq[String]) extends DebugStep[Seq[Variable]]
object LocalVariable {
  def inspect(variables: String*)(p: Seq[Variable] => Boolean)(implicit
      location: Location
  ): SingleStepAssert[Seq[Variable]] =
    new SingleStepAssert(new LocalVariable(variables), values => assert(p(values)))

  def apply(variables: String*)(expected: Seq[String])(implicit
      location: Location
  ): SingleStepAssert[Seq[Variable]] =
    new SingleStepAssert(new LocalVariable(variables), values => assertEquals(values.map(_.name).toSeq, expected))
}

object Outputed extends DebugStep[String] {
  def apply(expected: String): SingleStepAssert[String] =
    apply(message => assertEquals(message, expected))

  def apply(assertion: String => Unit): SingleStepAssert[String] =
    new SingleStepAssert(Outputed, assertion)
}

object NoStep extends DebugStep[Nothing] {
  def apply(): SingleStepAssert[Nothing] =
    new SingleStepAssert[Nothing](NoStep, _ => ())
}

case class Custom(f: () => Unit) extends DebugStep[Nothing]

object Custom {
  def apply(f: => Unit): SingleStepAssert[Nothing] = {
    new SingleStepAssert[Nothing](Custom(() => f), _ => ())
  }
}

object HotCodeReplace extends DebugStep[Array[StackFrame]] {
  def apply(expectedTopFrame: String): SingleStepAssert[Array[StackFrame]] =
    new SingleStepAssert(this, assertOnFrame(expectedTopFrame))
}

final case class ObjectRef(clsName: String)
