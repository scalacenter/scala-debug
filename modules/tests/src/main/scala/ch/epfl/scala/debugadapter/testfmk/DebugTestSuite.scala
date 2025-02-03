package ch.epfl.scala.debugadapter.testfmk

import ch.epfl.scala.debugadapter.DebugConfig
import ch.epfl.scala.debugadapter.DebugServer
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.GithubUtils
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.StepFiltersConfig
import com.microsoft.java.debug.core.protocol.Events.OutputEvent.Category
import com.microsoft.java.debug.core.protocol.Types.SourceBreakpoint
import com.microsoft.java.debug.core.protocol.Types.StackFrame
import com.microsoft.java.debug.core.protocol.Types.Variable

import java.net.URI
import java.util.concurrent.Executors
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.Properties

case class DebugCheckState(
    threadId: Long,
    topFrame: StackFrame,
    paused: Boolean
)

abstract class DebugTestSuite extends CommonFunSuite with DebugTest

// used by the scripted tests
object DebugTest extends DebugTest

trait DebugTest extends CommonUtils {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  val executorService = Executors.newFixedThreadPool(5)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(executorService)

  protected def defaultConfig: DebugConfig = DebugConfig.default.copy(testMode = true)

  def javaVersion: String = Properties.javaVersion

  def isScala3(implicit ctx: TestingContext) = ctx.scalaVersion.isScala3
  def isScala2(implicit ctx: TestingContext) = ctx.scalaVersion.isScala2
  def isScala213(implicit ctx: TestingContext) = ctx.scalaVersion.isScala213
  def isScala212(implicit ctx: TestingContext) = ctx.scalaVersion.isScala212
  def isScala33(implicit ctx: TestingContext) = ctx.scalaVersion.isScala33

  def getDebugServer(
      debuggee: Debuggee,
      config: DebugConfig = defaultConfig,
      logger: Logger = NoopLogger
  ): DebugServer =
    DebugServer(debuggee, TestingResolver, logger, config = config)

  def runDebugServer(
      debuggee: Debuggee,
      gracePeriod: Duration = 2.seconds,
      logger: Logger = NoopLogger
  ): DebugServer.Handler =
    DebugServer.run(debuggee, TestingResolver, logger, gracePeriod = gracePeriod)

  def check(uri: URI, attach: Option[Int] = None, stepFilters: StepFiltersConfig = null)(
      steps: DebugStepAssert*
  ): Unit = {
    val client = TestingDebugClient.connect(uri)
    try runAndCheck(client, attach, closeSession = true, stepFilters = stepFilters)(steps*)
    finally client.close()
  }

  def init(uri: URI)(steps: DebugStepAssert*): (TestingDebugClient, DebugCheckState) = {
    val client = TestingDebugClient.connect(uri)
    val state = runAndCheck(client, attach = None, closeSession = false)(steps*)
    (client, state)
  }

  def check(config: DebugConfig)(steps: DebugStepAssert*)(implicit debuggee: TestingDebuggee): Unit = {
    val server = getDebugServer(debuggee, config = config)
    val client = TestingDebugClient.connect(server.uri) // , logger = PrintLogger)
    try {
      server.connect()
      runAndCheck(client, attach = None, closeSession = true)(steps*)
    } finally {
      client.close()
      server.close()
    }
  }

  def check(steps: DebugStepAssert*)(implicit debuggee: TestingDebuggee): Unit = {
    val server = getDebugServer(debuggee) // , logger = PrintLogger)
    val client = TestingDebugClient.connect(server.uri) // , logger = PrintLogger)
    try {
      server.connect()
      runAndCheck(client, attach = None, closeSession = true)(steps*)
    } finally {
      client.close()
      server.close()
    }
  }

  private def runAndCheck(
      client: TestingDebugClient,
      attach: Option[Int],
      closeSession: Boolean,
      stepFilters: StepFiltersConfig = null
  )(
      steps: DebugStepAssert*
  ): DebugCheckState = {
    client.initialize()
    attach match {
      case Some(port) => client.attach("localhost", port)
      case None => client.launch(stepFilters = stepFilters)
    }

    val sourceBreakpoints = steps
      .collect { case s: SingleStepAssert[?] => s.step }
      .distinct
      .collect {
        case b: Breakpoint =>
          b.sourceFile -> new SourceBreakpoint(b.line, b.condition.getOrElse(null), null)
        case l: Logpoint =>
          val breakpoint = new SourceBreakpoint(l.line, null, null)
          breakpoint.logMessage = l.logMessage
          l.sourceFile -> breakpoint
      }

    sourceBreakpoints
      .groupBy(_._1)
      .map { case (sourceFile, bs) => (sourceFile, bs.map(_._2)) }
      .foreach { case (sourceFile, sourceBreakpoints) =>
        sourceBreakpoints
          .groupBy(_.line)
          .foreach { case (line, bs) =>
            assert(bs.size == 1, s"More than one breakpoint in $sourceFile on line $line")
          }
        val configuredBreakpoints = client.setSourceBreakpoints(sourceFile, sourceBreakpoints)
        assert(configuredBreakpoints.length == sourceBreakpoints.length)
        def unverifiedBreakpoint =
          configuredBreakpoints.find(!_.verified).get
        assert(
          configuredBreakpoints.forall(_.verified),
          s"Unverified breakpoint in ${sourceFile.getFileName} at line ${unverifiedBreakpoint.line}"
        )
      }
    client.configurationDone()

    runAndCheck(client, DebugCheckState(-1, null, false), closeSession)(steps: _*)
  }

  def runAndCheck(client: TestingDebugClient, initialState: DebugCheckState, closeSession: Boolean)(
      steps: DebugStepAssert*
  ): DebugCheckState = {
    var state = initialState

    def evaluate(expr: String, assertion: Either[String, String] => Unit): Future[Unit] = {
      client.evaluate(expr, state.topFrame.id).map { resp =>
        println(s"$$ $expr")
        resp.foreach(res => println(s"> $res"))
        resp.left.foreach(err => println(s"> $err"))
        assertion(resp)
      }
    }

    def inspect(variable: LocalVariable, assertion: Seq[Variable] => Unit): Unit = {
      val values = for {
        localScopeRef <- client.scopes(state.topFrame.id).find(_.name == "Local").map(_.variablesReference).toSeq
        variableRefOpt: Option[Int] = variable.names.foldLeft(Option(localScopeRef))((ref, name) =>
          ref.toSeq.flatMap(x => client.variables(x)).find(_.name == name).map(_.variablesReference)
        )
        variableRef <- variableRefOpt.toSeq
        v <- client.variables(variableRef)
      } yield v
      assertion(values)
    }

    def stop(): Array[StackFrame] = {
      val stopped = client.stopped()
      val threadId = stopped.threadId
      val stackTrace = client.stackTrace(threadId)

      state = state.copy(threadId = threadId, topFrame = stackTrace.stackFrames.head, paused = true)
      stackTrace.stackFrames
    }

    def continueIfPaused(): Unit = if (state.paused) {
      client.continue(state.threadId)
      state = state.copy(paused = false)
    }

    steps.foreach {
      case SingleStepAssert(_: Breakpoint, assertion) =>
        continueIfPaused()
        assertion(stop())
      case SingleStepAssert(_: Logpoint, assertion) =>
        continueIfPaused()
        // A log point needs time for evaluation
        val event = client.outputed(m => m.category == Category.stdout, defaultTimeout(16.seconds))
        print(s"> ${event.output}")
        assertion(event.output.trim)
      case SingleStepAssert(StepIn, assertion) =>
        println(s"Stepping in, at ${state.topFrame.name}")
        client.stepIn(state.threadId)
        assertion(stop())
      case SingleStepAssert(StepOut, assertion) =>
        println(s"Stepping out, at ${state.topFrame.name}")
        client.stepOut(state.threadId)
        assertion(stop())
      case SingleStepAssert(StepOver, assertion) =>
        println(s"Stepping over, at ${state.topFrame.name}")
        client.stepOver(state.threadId)
        assertion(stop())
      case SingleStepAssert(Evaluation(expr), assertion) =>
        Await.result(evaluate(expr, assertion), defaultTimeout(16.seconds))
      case SingleStepAssert(Outputed, assertion) =>
        continueIfPaused()
        val event = client.outputed(m => m.category == Category.stdout)
        print(s"> ${event.output}")
        assertion(event.output.trim)
      case SingleStepAssert(NoStep, _) => ()
      case ParallelStepsAsserts(steps) =>
        val evaluations = steps.collect { case SingleStepAssert(Evaluation(expr), assertion) =>
          evaluate(expr, assertion)
        }
        Await.result(Future.sequence(evaluations), defaultTimeout(64.seconds))
      case SingleStepAssert(localVariable: LocalVariable, assertion) =>
        inspect(localVariable, assertion)
      case SingleStepAssert(Custom(f), _) => f()
      case SingleStepAssert(HotCodeReplace, assertion) =>
        client.redefineClasses()
        assertion(stop())
    }

    if (closeSession) {
      continueIfPaused()
      if (!GithubUtils.isCI()) {
        // client.exited(timeout = 4.seconds)
        // client.terminated(timeout = 4.seconds)
      }
    }

    state
  }
}
