package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider

import java.io.Closeable
import java.io.File
import java.nio.file.Path

trait Debuggee {
  def name: String
  def scalaVersion: ScalaVersion
  def run(listener: DebuggeeListener): CancelableFuture[Unit]
  def modules: Seq[Module]
  def libraries: Seq[Library]
  def unmanagedEntries: Seq[UnmanagedEntry]
  def javaRuntime: Option[JavaRuntime]
  def observeClassUpdates(onClassUpdate: Seq[String] => Unit): Closeable

  def managedEntries: Seq[ManagedEntry] = modules ++ libraries
  def classPathEntries: Seq[ClassPathEntry] = managedEntries ++ unmanagedEntries
  def classPath: Seq[Path] = classPathEntries.map(_.absolutePath)
  def classEntries: Seq[ClassEntry] = classPathEntries ++ javaRuntime
  def classPathString: String = classPath.mkString(File.pathSeparator)

  @volatile private[debugadapter] var sourceLookUpProvider: Option[SourceLookUpProvider] = None

  private[debugadapter] def setSourceLookUpProvider(provider: SourceLookUpProvider): Unit = {
    sourceLookUpProvider = Some(provider)
  }
}
