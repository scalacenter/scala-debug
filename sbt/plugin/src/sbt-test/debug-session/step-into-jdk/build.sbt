import scala.util.Properties
import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient
import java.nio.file.Paths

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

val jdkHome = Paths.get(Properties.jdkHome)
val jdkSourceZip = jdkHome.resolve("src.zip")

scalaVersion := "2.13.6"
javaHome := Some(jdkHome.toFile)
checkDebugSession := {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath
  val logger = streams.value.log

  val client = TestDebugClient.connect(uri)
  try {
    client.initialize()
    client.launch()

    val breakpoints = client.setBreakpoints(source, Array(5))
    assert(breakpoints.size == 1)
    assert(breakpoints.forall(_.verified))

    client.configurationDone()

    val threadId = client.stopped().threadId

    client.stepIn(threadId)
    client.stopped()

    val frame = client.stackTrace(threadId).stackFrames.head
    assert(frame.source.path.endsWith("/java/io/PrintStream.java"))

    client.continue(threadId)

    client.exited()
    client.terminated()
  } finally {
    client.close()
  }
}
