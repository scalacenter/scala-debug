package ch.epfl.scala.debugadapter.sbtplugin.internal

import ch.epfl.scala.debugadapter.*
import ch.epfl.scala.debugadapter.sbtplugin.{AnnotatedFingerscan, SubclassFingerscan}
import sbt.Tests.Cleanup
import sbt.internal.bsp.BuildTargetIdentifier
import sbt.io.IO
import sbt.testing.*
import sbt.{ForkConfiguration, ForkMain, ForkOptions, ForkTags, TestDefinition, TestFramework}

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.net.{ServerSocket, Socket}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import ch.epfl.scala.debugadapter.testing.TestSuiteEvent
import io.reactivex.Observable
import java.io.Closeable

private[debugadapter] sealed trait SbtDebuggee extends Debuggee {
  val logger: LoggerAdapter
  val classesToUpdate: Observable[Seq[String]]

  override def observeClassUpdates(onClassUpdate: Seq[String] => Unit): Closeable = {
    val subscription = classesToUpdate.subscribe(onClassUpdate(_))
    () => if (!subscription.isDisposed) subscription.dispose
  }
}

private[debugadapter] final class MainClassDebuggee(
    target: BuildTargetIdentifier,
    val scalaVersion: ScalaVersion,
    forkOptions: ForkOptions,
    val modules: Seq[Module],
    val libraries: Seq[Library],
    val unmanagedEntries: Seq[UnmanagedEntry],
    val javaRuntime: Option[JavaRuntime],
    override val classesToUpdate: Observable[Seq[String]],
    mainClass: String,
    args: Seq[String],
    val logger: LoggerAdapter
)(implicit ec: ExecutionContext)
    extends SbtDebuggee {
  override def name: String =
    s"${getClass.getSimpleName}(${target.uri}, $mainClass)"

  override def run(listener: DebuggeeListener): CancelableFuture[Unit] =
    DebuggeeProcess.start(forkOptions, classPath, mainClass, args, listener, logger)
}

private[debugadapter] final class TestSuitesDebuggee(
    target: BuildTargetIdentifier,
    val scalaVersion: ScalaVersion,
    forkOptions: ForkOptions,
    val modules: Seq[Module],
    val libraries: Seq[Library],
    val unmanagedEntries: Seq[UnmanagedEntry],
    val javaRuntime: Option[JavaRuntime],
    override val classesToUpdate: Observable[Seq[String]],
    cleanups: Seq[Cleanup],
    parallel: Boolean,
    runners: Map[TestFramework, Runner],
    tests: Seq[TestDefinition],
    val logger: LoggerAdapter
)(implicit executionContext: ExecutionContext)
    extends SbtDebuggee {

  override def name: String =
    s"${getClass.getSimpleName}(${target.uri}, [${tests.mkString(", ")}])"

  override def run(listener: DebuggeeListener): CancelableFuture[Unit] = {
    val eventHandler = new SbtTestSuiteEventHandler(listener, () => sourceLookUpProvider)

    @annotation.tailrec
    def receiveLogs(is: ObjectInputStream, os: ObjectOutputStream): Unit = {
      is.readObject() match {
        case Array(ForkTags.Error, s: String) =>
          eventHandler.handle(TestSuiteEvent.Error(s))
          receiveLogs(is, os)
        case Array(ForkTags.Warn, s: String) =>
          eventHandler.handle(TestSuiteEvent.Warn(s))
          receiveLogs(is, os)
        case Array(ForkTags.Info, s: String) =>
          eventHandler.handle(TestSuiteEvent.Info(s))
          receiveLogs(is, os)
        case Array(ForkTags.Debug, s: String) =>
          eventHandler.handle(TestSuiteEvent.Debug(s))
          receiveLogs(is, os)
        case t: Throwable =>
          eventHandler.handle(TestSuiteEvent.Trace(t))
          receiveLogs(is, os)
        case Array(testSuite: String, events: Array[Event]) =>
          eventHandler.handle(TestSuiteEvent.Results(testSuite, events.toList))
          receiveLogs(is, os)
        case ForkTags.Done =>
          eventHandler.handle(TestSuiteEvent.Done)
          os.writeObject(ForkTags.Done)
          os.flush()
      }
    }

    object Acceptor extends Thread {
      val server = new ServerSocket(0)
      var socket: Socket = _
      var os: ObjectOutputStream = _
      var is: ObjectInputStream = _

      override def run(): Unit = {
        try {
          socket = server.accept()
          os = new ObjectOutputStream(socket.getOutputStream)
          is = new ObjectInputStream(socket.getInputStream)

          // Must flush the header that the constructor writes
          // otherwise the ObjectInputStream on the other end may block indefinitely
          os.flush()

          val config = new ForkConfiguration(true, parallel)
          os.writeObject(config)

          val taskdefs = tests.map { t =>
            val forkFingerprint = t.fingerprint match {
              case s: SubclassFingerprint => new SubclassFingerscan(s)
              case a: AnnotatedFingerprint => new AnnotatedFingerscan(a)
              case f => sys.error("Unknown fingerprint type: " + f.getClass)
            }
            new TaskDef(
              t.name,
              forkFingerprint,
              t.explicitlySpecified,
              t.selectors
            )
          }
          os.writeObject(taskdefs.toArray)

          os.writeInt(runners.size)
          for ((testFramework, mainRunner) <- runners) {
            os.writeObject(testFramework.implClassNames.toArray)
            os.writeObject(mainRunner.args)
            os.writeObject(mainRunner.remoteArgs)
          }
          os.flush()
          receiveLogs(is, os)
        } catch {
          case NonFatal(e) => close()
        }
      }

      def close(): Unit = {
        if (os != null) os.close()
        if (socket != null) socket.close()
        server.close()
      }
    }

    Acceptor.start()

    val mainClass = classOf[ForkMain].getCanonicalName
    val args = Seq(Acceptor.server.getLocalPort.toString)

    val fullClasspath = classPath ++ Seq(
      IO.classLocationPath[ForkMain], // test-agent
      IO.classLocationPath[Framework], // test-interface
      IO.classLocationPath[SubclassFingerscan] // sbt-plugin
    )

    val process = DebuggeeProcess.start(forkOptions, fullClasspath, mainClass, args, listener, logger)
    process.future.onComplete { _ =>
      Acceptor.close()
      try {
        // the setup happens during the execution of the test
        // the cleanup can crash if it needs the sbt streams (which is already closed)
        val dummyLoader = getClass.getClassLoader
        cleanups.foreach(_.cleanup(dummyLoader))
      } catch {
        case NonFatal(cause) =>
          logger.warn(s"Failed to cleanup the tests")
          logger.trace(cause)
      }
    }

    process
  }
}

private[debugadapter] final class AttachRemoteDebuggee(
    target: BuildTargetIdentifier,
    val scalaVersion: ScalaVersion,
    val modules: Seq[Module],
    val libraries: Seq[Library],
    val unmanagedEntries: Seq[UnmanagedEntry],
    val javaRuntime: Option[JavaRuntime],
    override val classesToUpdate: Observable[Seq[String]],
    val logger: LoggerAdapter
) extends SbtDebuggee {
  override def name: String = s"${getClass.getSimpleName}(${target.uri})"
  override def run(listener: DebuggeeListener): CancelableFuture[Unit] =
    new CancelableFuture[Unit] {
      override def future: Future[Unit] = Future.successful(())
      override def cancel(): Unit = ()
    }
}
