package ch.epfl.scala.debugadapter.internal

import coursier._
import java.io.File
import ch.epfl.scala.debugadapter.SourceJar
import ch.epfl.scala.debugadapter.Library
import ch.epfl.scala.debugadapter.testfmk.NoopLogger
import munit.FunSuite
import scala.util.Properties

class SourceLookUpProviderSpec extends FunSuite {
  test("fix https://github.com/scalameta/metals/issues/3477#issuecomment-1013458270") {
    assume(!Properties.isMac) // TODO not working on MacOS
    val artifacts = coursier
      .Fetch()
      .addDependencies(Dependency(Module(Organization("org.openjfx"), ModuleName("javafx-controls")), "17.0.1"))
      .addClassifiers(Classifier.sources, Classifier("win"), Classifier("linux"), Classifier("mac"))
      .withMainArtifacts()
      .run()

    val classPath = artifacts
      .groupBy(getArtifactId)
      .values
      .flatMap { group =>
        val sourcesJar = group.find(_.getName.endsWith("-sources.jar")).get
        val winJar = group.find(_.getName.endsWith("-win.jar")).get
        val linuxJar = group.find(_.getName.endsWith("-linux.jar")).get
        val macJar = group.find(_.getName.endsWith("-mac.jar")).get
        val mainJar = group
          .find(file => !Set(sourcesJar, winJar, linuxJar, macJar).contains(file))
          .get
        val sourceEntries = Seq(SourceJar(sourcesJar.toPath))
        Seq(winJar, linuxJar, macJar, mainJar)
          .map(_.toPath)
          .map(path => Library("javafx-controls", "17.0.1", path, sourceEntries))
      }
      .toSeq

    for (_ <- 0 until 10) SourceLookUpProvider(classPath, NoopLogger)
  }

  private def getArtifactId(file: File): String = {
    file.getName
      .stripSuffix(".jar")
      .stripSuffix("-sources")
      .stripSuffix("-win")
      .stripSuffix("-linux")
      .stripSuffix("-mac")
  }
}
