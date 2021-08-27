import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient

val checkDebugSession = inputKey[Unit]("Check the test suite debug session")

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.10" % Test
testFrameworks += new TestFramework("utest.runner.Framework")
scalaVersion := "2.13.6"
checkDebugSession := {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val uri = (Test / startTestSuitesDebugSession).evaluated
  val mainSource = (Compile / sources).value.head.toPath
  val specSource = (Test / sources).value.head.toPath

  val client = TestDebugClient.connect(uri)
  try {
    client.initialize()
    client.launch()

    val breakpoints = client.setBreakpoints(mainSource, Array(3)) ++
      client.setBreakpoints(specSource, Array(6, 8))
    assert(breakpoints.size == 3)
    assert(breakpoints.forall(_.verified))

    client.configurationDone()

    val threadId = client.stopped().threadId

    client.continue(threadId)
    client.stopped()

    client.continue(threadId)
    client.stopped()

    client.continue(threadId)
    client.exited()
    client.terminated()
  } finally {
    client.close()
  }
}
