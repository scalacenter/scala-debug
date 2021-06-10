import sbt._

object Dependencies {
  val scala212 = "2.12.12"
  val asmVersion = "7.0"

  val asm = "org.ow2.asm" % "asm" % asmVersion
  val asmUtil = "org.ow2.asm" % "asm-util" % asmVersion
  val javaDebug = "ch.epfl.scala" % "com-microsoft-java-debug-core" % "0.21.0+1-7f1080f1"
  val utest = "com.lihaoyi" %% "utest" % "0.6.6"
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % scala212
  val sbtIo = "org.scala-sbt" %% "io" % "1.4.0"
  val sbtTestInterface = "org.scala-sbt" % "test-interface" % "1.0"
  val coursier = "io.get-coursier" %% "coursier" % "2.0.16"
}
