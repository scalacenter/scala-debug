package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.{ClassEntry, Logger, Module}
import ch.epfl.scala.debugadapter.internal.scalasig.ScalaSig
import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider

import java.net.URI
import scala.collection.parallel.immutable.ParVector

private[debugadapter] final class SourceLookUpProvider(
    private[internal] var classPathEntries: Seq[ClassEntryLookUp],
    private var sourceUriToClassPathEntry: Map[SourceFileKey, ClassEntryLookUp],
    private var fqcnToClassPathEntry: Map[String, ClassEntryLookUp],
    logger: Logger
) extends ISourceLookUpProvider {
  var classesByScalaNameMap: Map[String, Seq[String]] = loadClassesByScalaName

  override def supportsRealtimeBreakpointVerification(): Boolean = true

  override def getSourceFileURI(fqcn: String, path: String): String = {
    getSourceFileByClassname(fqcn).map(_.toString).orNull
  }

  def getSourceFileURI(fileName: String): Option[URI] = {
    classPathEntries.iterator.map(_.uriFromFilename(fileName)).collectFirst { case Some(value) =>
      value
    }
  }

  override def getSourceContents(uri: String): String = {
    val sourceUri = URI.create(uri)
    sourceUriToClassPathEntry
      .get(SourceFileKey(sourceUri))
      .flatMap(_.getSourceContent(sourceUri))
      .orNull
  }

  override def getFullyQualifiedName(
      uriRepr: String,
      lines: Array[Int],
      columns: Array[Int]
  ): Array[String] = {
    val uri = URI.create(uriRepr)

    uri.getScheme match {
      case "dap-fqcn" =>
        val resolvedName = uri.getSchemeSpecificPart
        lines.map(_ => resolvedName)
      case _ =>
        val key = SourceFileKey(uri);
        sourceUriToClassPathEntry.get(key) match {
          case None => lines.map(_ => null)
          case Some(entry) =>
            lines.map(line => entry.getFullyQualifiedClassName(key, line).orNull)
        }
    }
  }

  def getClassFile(fqcn: String): Option[ClassFile] =
    fqcnToClassPathEntry.get(fqcn).flatMap(_.getClassFile(fqcn))

  def getClassEntry(fqcn: String): Option[ClassEntry] =
    fqcnToClassPathEntry.get(fqcn).map(_.entry)

  def getSourceContentFromClassName(fqcn: String): Option[String] =
    fqcnToClassPathEntry.get(fqcn).flatMap(_.getSourceContentFromClassName(fqcn))

  def containsClass(fqcn: String): Boolean =
    fqcnToClassPathEntry.contains(fqcn)

  private[internal] def allClassNames: Iterable[String] =
    classPathEntries.flatMap(_.fullyQualifiedNames)
  private[internal] def allOrphanClasses: Iterable[ClassFile] =
    classPathEntries.flatMap(_.orphanClassFiles)
  private[internal] def classesByScalaName(name: String): Seq[String] =
    classesByScalaNameMap.get(name).getOrElse(Seq.empty)

  private[internal] def getScalaSig(fqcn: String): Option[ScalaSig] = {
    for {
      classPathEntry <- fqcnToClassPathEntry.get(fqcn)
      scalaSig <- classPathEntry.getScalaSig(fqcn)
    } yield scalaSig
  }

  private def getSourceFileByClassname(className: String): Option[URI] = {
    fqcnToClassPathEntry
      .get(className)
      .flatMap(_.getSourceFileURI(className))
  }

  def reload(classesToReplace: Seq[String]): Unit = {
    classPathEntries = classPathEntries.map { lookUp =>
      lookUp.entry match {
        case m: Module => ClassEntryLookUp(m, logger)
        case _ => lookUp
      }
    }

    sourceUriToClassPathEntry = classPathEntries.flatMap(lookup => lookup.sources.map(uri => (uri, lookup))).toMap
    fqcnToClassPathEntry =
      classPathEntries.flatMap(lookup => lookup.fullyQualifiedNames.map(fqcn => (fqcn, lookup))).toMap
    classesByScalaNameMap = loadClassesByScalaName
  }

  private def loadClassesByScalaName: Map[String, Seq[String]] =
    // Note: This can be a hotspot as there can be a LOT of classes. Keep perf in mind when refactoring
    classPathEntries.map(_.classesByScalaName).foldLeft(Map.empty[String, Seq[String]]) { (accum, currMap) =>
      currMap.foldLeft(accum) { (accumInner, kv) =>
        {
          val (k, v) = kv
          accumInner.updated(k, accumInner.getOrElse(k, Seq.empty) ++ v)
        }
      }
    }
}

private[debugadapter] object SourceLookUpProvider {
  def empty(logger: Logger): SourceLookUpProvider =
    new SourceLookUpProvider(Seq.empty, Map.empty, Map.empty, logger)

  def apply(entries: Seq[ClassEntry], logger: Logger): SourceLookUpProvider = {
    val parallelEntries = ParVector(entries*)
    val sourceLookUps =
      parallelEntries
        .flatMap(_.sourceEntries)
        .distinct
        .map(entry => entry -> SourceEntryLookUp(entry, logger))
        .toMap
    val allLookUps = parallelEntries
      .map(entry => ClassEntryLookUp(entry, entry.sourceEntries.flatMap(sourceLookUps.apply), logger))
      .seq
      .reverse
    val sourceUriToClassPathEntry = allLookUps
      .flatMap(lookup => lookup.sources.map(uri => (uri, lookup)))
      .toMap
    val fqcnToClassPathEntry = allLookUps
      .flatMap(lookup => lookup.fullyQualifiedNames.map(fqcn => (fqcn, lookup)))
      .toMap
    new SourceLookUpProvider(allLookUps, sourceUriToClassPathEntry, fqcnToClassPathEntry, logger)
  }
}
