import sbt._

object Dependencies {
  val scalaEnvVersion = Option(System.getenv("SCALA_VERSION"))
  val scala212 = scalaEnvVersion.filter(isScala212).getOrElse("2.12.20")
  val scala213 = scalaEnvVersion.filter(isScala213).getOrElse("2.13.16")
  val scala30 = scalaEnvVersion.filter(isScala30).getOrElse("3.0.2")
  val scala31Plus = scalaEnvVersion.filter(isScala31Plus).getOrElse("3.3.4")
  val scala34Plus = scalaEnvVersion.filter(isScala34Plus).getOrElse("3.6.2")
  val asmVersion = "9.7.1"
  val coursierVersion = "2.1.24"

  def isScala212(version: String): Boolean = version.startsWith("2.12")
  def isScala213(version: String): Boolean = version.startsWith("2.13")
  def isScala30(version: String): Boolean = version.startsWith("3.0")
  def isScala31Plus(version: String): Boolean = SemVer.matches(version) { case (3, 1 | 2 | 3, _) => true }
  def isScala34Plus(version: String): Boolean = SemVer.matches(version) { case (3, min, _) => min >= 4 }

  val asm = "org.ow2.asm" % "asm" % asmVersion
  val asmUtil = "org.ow2.asm" % "asm-util" % asmVersion

  def scalaCompiler(scalaVersion: String): ModuleID =
    SemVer(scalaVersion) match {
      case (3, _, _) => "org.scala-lang" %% "scala3-compiler" % scalaVersion
      case _ => "org.scala-lang" % "scala-compiler" % scalaVersion
    }

  def scalaReflect(scalaVersion: String): ModuleID =
    SemVer(scalaVersion) match {
      case (3, _, _) => "org.scala-lang" % "scala-reflect" % scala213
      case _ => "org.scala-lang" % "scala-reflect" % scalaVersion
    }

  val scalaParallelCollection = "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
  val scalaCollectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.13.0"
  val sbtTestAgent = "org.scala-sbt" % "test-agent" % "1.10.7"
  val scalaMeta = ("org.scalameta" %% "parsers" % "4.12.7").cross(CrossVersion.for3Use2_13)

  // test dependencies
  val munit = "org.scalameta" %% "munit" % "1.1.0"
  val coursier = "io.get-coursier" %% "coursier" % coursierVersion
  val coursierJvm = "io.get-coursier" %% "coursier-jvm" % coursierVersion
}
