package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.*
import ch.epfl.scala.debugadapter.testing.TestSuiteSummary
import com.microsoft.java.debug.core.adapter.ProtocolServer
import com.microsoft.java.debug.core.protocol.Events
import com.microsoft.java.debug.core.protocol.Events.OutputEvent
import com.microsoft.java.debug.core.protocol.JsonUtils
import com.microsoft.java.debug.core.protocol.Messages.Request
import com.microsoft.java.debug.core.protocol.Messages.Response
import com.microsoft.java.debug.core.protocol.Requests.*

import java.net.InetSocketAddress
import java.net.Socket
import java.util.concurrent.CancellationException
import java.util.concurrent.TimeoutException
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider

/**
 * This debug adapter maintains the lifecycle of the debuggee in separation from JDI.
 * The debuggee is started/closed together with the session.
 *
 * This approach makes it necessary to handle the "launch" requests as the "attach" ones.
 * The JDI address of the debuggee is obtained through the [[DebuggeeListener]]
 */
private[debugadapter] final class DebugSession private (
    socket: Socket,
    debuggee: Debuggee,
    context: ScalaProviderContext,
    resolver: DebugToolsResolver,
    logger: Logger,
    loggingAdapter: LoggingAdapter,
    config: DebugConfig
)(implicit executionContext: ExecutionContext)
    extends ProtocolServer(
      socket.getInputStream,
      socket.getOutputStream,
      context,
      loggingAdapter.factory
    ) {
  private type LaunchId = Int

  // A set of all processed launched requests by the client
  private val launchedRequests = mutable.Set.empty[LaunchId]

  private val terminatedEvent = Promise[Unit]()
  private val debuggeeAddress = Promise[InetSocketAddress]()
  private val attached = Promise[Unit]()

  private val exitStatusPromise = Promise[DebugSession.ExitStatus]()
  private val debugState: Synchronized[DebugSession.State] = new Synchronized(
    DebugSession.Ready
  )

  private[debugadapter] def currentState: DebugSession.State = debugState.value

  private[debugadapter] def getDebuggeeAddress: Future[InetSocketAddress] =
    debuggeeAddress.future

  /**
   * Schedules the start of the debugging session.
   *
   * For a session to start, two executions must happen independently in a
   * non-blocking way: the debuggee process is started in the background and
   * the DAP server starts listening to client requests in an IO thread.
   */
  private[debugadapter] def start(): Unit = {
    debugState.transform {
      case DebugSession.Ready =>
        DebugSession.fork(run)
        val process = debuggee.run(Listener)

        process.future
          .onComplete { result =>
            result.failed.foreach(cancelPromises)
            // wait for the terminated event then close the session
            terminatedEvent.future.map { _ =>
              exitStatusPromise.trySuccess(DebugSession.Terminated)
            }
          }

        DebugSession.Started(process)

      case otherState =>
        otherState // don't start if already started or cancelled
    }
  }

  /**
   * Completed, once this session exit status can be determined.
   * Those are: [[DebugSession.Terminated]], [[DebugSession.Restarted]] and [[DebugSession.Disconnected]].
   * <p>Session gets the Terminated status when the communication stops before
   * the client has requested a disconnection.</p>
   * <p>Session becomes Restarted immediately when the disconnection request is received
   * and restart is set to true.</p>
   * <p>Session becomes Disconnected immediately when the disconnection request is received
   * and restart is set to false.</p>
   */
  def exitStatus: Future[DebugSession.ExitStatus] = exitStatusPromise.future

  /**
   * Cancel the debuggee process, stop the DAP server and close the socket.
   */
  def close(): Unit = {
    super.stop()
    loggingAdapter.onClosingSession()
    debugState.transform {
      case DebugSession.Started(debuggee) =>
        cancelPromises(new CancellationException("Debug session closed"))
        debuggee.cancel()

        // Wait for the debuggee to terminate gracefully
        try Await.result(terminatedEvent.future, config.gracePeriod)
        catch {
          case _: TimeoutException =>
            logger.warn(
              s"Communication with debuggee $name is frozen: missing terminated event."
            )
        }
        DebugSession.Stopped

      case _ => DebugSession.Stopped
    }
    context.getProviders.forEach(_.close())
    logger.debug(s"Closing connection with debugger $name")
    socket.close()
    exitStatusPromise.trySuccess(DebugSession.Terminated)
  }

  protected override def dispatchRequest(request: Request): Unit = {
    val requestId = request.seq
    request.command match {
      case "attach" =>
        val tools = DebugTools(debuggee, resolver, logger)
        context.configure(tools, config)
        super.dispatchRequest(request)
      case "launch" =>
        val command = Command.parse(request.command)
        // the launch args sent by Metals do not conform to the LaunchArgument of java-debug
        // here we parse to PartialLaunchArguments which only contains noDebug
        val launchArgs = JsonUtils.fromJson(request.arguments, classOf[ScalaLaunchArguments])
        val tools =
          if (launchArgs.noDebug) DebugTools.none(logger)
          else DebugTools(debuggee, resolver, logger)
        val debugConfig =
          if (launchArgs.scalaStepFilters == null) config else config.copy(stepFilters = launchArgs.scalaStepFilters)
        context.configure(tools, debugConfig)
        context.getProvider(classOf[ISourceLookUpProvider]) match {
          case provider: SourceLookUpProvider =>
            debuggee.setSourceLookUpProvider(provider)
        }
        // launch request is implemented by spinning up a JVM
        // and sending an attach request to the java DapServer
        launchedRequests.add(requestId)
        Scheduler
          .timeout(debuggeeAddress, config.gracePeriod)
          .future
          .onComplete {
            case Success(address) =>
              super.dispatchRequest(
                DebugSession.toAttachRequest(requestId, address)
              )
            case Failure(exception) =>
              val cause =
                s"Could not start debuggee $name due to: ${exception.getMessage}"
              this.sendResponse(DebugSession.failed(request, cause))
              attached.tryFailure(new IllegalStateException(cause))
              ()
          }

      case "configurationDone" =>
        // Delay handling of this request until we attach to the debuggee.
        // Otherwise, a race condition may happen when we try to communicate
        // with the VM we are not connected to
        attached.future
          .onComplete {
            case Success(_) =>
              super.dispatchRequest(request)
            case Failure(exception) =>
              sendResponse(DebugSession.failed(request, exception.getMessage))
          }

      case "disconnect" =>
        debugState.transform {
          case DebugSession.Started(debuggee) =>
            cancelPromises(new CancellationException("Client disconnected"))
            exitStatusPromise.trySuccess {
              if (DebugSession.shouldRestart(request)) DebugSession.Restarted
              else DebugSession.Disconnected
            }
            super.dispatchRequest(request)
            debuggee.cancel()
            DebugSession.Stopped
          case _ =>
            val ack = new Response(request.seq, request.command, true)
            sendResponse(ack)
            DebugSession.Stopped
        }

      case _ => super.dispatchRequest(request)
    }
  }

  override def sendResponse(response: Response): Unit = {
    val requestId = response.request_seq
    response.command match {
      case "attach" if launchedRequests(requestId) =>
        // attach response from java DapServer is transformed into a launch response
        // that is forwarded to the DAP client
        response.command = Command.LAUNCH.getName
        attached.success(())
        super.sendResponse(response)
      case "attach" =>
        // a response to an actual attach request sent by a DAP client
        attached.success(())
        super.sendResponse(response)
      case _ =>
        super.sendResponse(response)
    }
  }

  override def sendEvent(event: Events.DebugEvent): Unit = {
    // delay terminated and exited events to fix the ordering of events
    try {
      event.`type` match {
        case "terminated" => Thread.sleep(200)
        case "exited" => Thread.sleep(100)
        case _ => ()
      }
      super.sendEvent(event)
    } finally if (event.`type` == "terminated") terminatedEvent.trySuccess(())
  }

  private def name = debuggee.name

  private def cancelPromises(cause: Throwable): Unit = {
    debuggeeAddress.tryFailure(cause)
    attached.tryFailure(cause)
  }

  private object Listener extends DebuggeeListener {
    override def onListening(address: InetSocketAddress): Unit = {
      debuggeeAddress.trySuccess(address)
    }

    override def out(line: String): Unit = {
      val event = new OutputEvent(
        OutputEvent.Category.stdout,
        line + System.lineSeparator()
      )
      sendEvent(event)
    }

    override def err(line: String): Unit = {
      val event = new OutputEvent(
        OutputEvent.Category.stderr,
        line + System.lineSeparator()
      )
      sendEvent(event)
    }

    override def testResult(data: TestSuiteSummary): Unit = {
      val event = TestResultEvent(data)
      sendEvent(event)
    }
  }
}

private[debugadapter] object DebugSession {
  sealed trait ExitStatus

  /**
   * The debugger has asked for a restart
   */
  case object Restarted extends ExitStatus

  /**
   * The debugger has disconnected
   */
  case object Disconnected extends ExitStatus

  /**
   * The debuggee has terminated
   */
  case object Terminated extends ExitStatus

  sealed trait State
  case object Ready extends State
  case class Started(process: CancelableFuture[Unit]) extends State
  case object Stopped extends State

  def apply(
      socket: Socket,
      debuggee: Debuggee,
      resolver: DebugToolsResolver,
      logger: Logger,
      config: DebugConfig
  )(implicit executionContext: ExecutionContext): DebugSession = {
    val loggingHandler = new LoggingAdapter(logger)
    val context = ScalaProviderContext(debuggee, logger, config)
    new DebugSession(socket, debuggee, context, resolver, logger, loggingHandler, config)
  }

  private def fork(f: () => Unit): Unit = {
    val thread = new Thread {
      override def run(): Unit = f()
    }
    thread.start()
  }

  private def toAttachRequest(seq: Int, address: InetSocketAddress): Request = {
    val arguments = new AttachArguments
    arguments.hostName = address.getHostName
    arguments.port = address.getPort

    val json = JsonUtils.toJsonTree(arguments, classOf[AttachArguments])
    new Request(seq, Command.ATTACH.getName, json.getAsJsonObject)
  }

  private def failed(request: Request, message: String): Response = {
    new Response(request.seq, request.command, false, message)
  }

  private def shouldRestart(disconnectRequest: Request): Boolean = {
    Try(
      JsonUtils.fromJson(
        disconnectRequest.arguments,
        classOf[DisconnectArguments]
      )
    )
      .map(_.restart)
      .getOrElse(false)
  }
}
