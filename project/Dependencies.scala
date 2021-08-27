import sbt._

object Dependencies {
  val scala212 = "2.13.6"
  val asmVersion = "9.2"
  val coursierVersion = "2.0.16"

  val asm = "org.ow2.asm" % "asm" % asmVersion
  val asmUtil = "org.ow2.asm" % "asm-util" % asmVersion
  val javaDebug =
    "ch.epfl.scala" % "com-microsoft-java-debug-core" % "0.21.0+1-7f1080f1"
  val utest = "com.lihaoyi" %% "utest" % "0.7.10"
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % scala212
  val sbtIo = "org.scala-sbt" %% "io" % "1.5.1"
  val sbtTestInterface = "org.scala-sbt" % "test-interface" % "1.0"
  val coursier = "io.get-coursier" %% "coursier" % coursierVersion
  val coursierJvm = "io.get-coursier" %% "coursier-jvm" % coursierVersion
}
