package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter._
import ch.epfl.scala.debugadapter.internal.ScalaExtension.*
import ch.epfl.scala.debugadapter.internal.scalasig.Decompiler
import ch.epfl.scala.debugadapter.internal.scalasig.ScalaSig
import org.objectweb.asm._

import java.net.URI
import java.nio.file._
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Properties
import scala.util.Try
import scala.util.matching.Regex

import ClassEntryLookUp.readSourceContent

private[internal] case class ClassFile(
    fullyQualifiedName: String,
    sourceName: Option[String],
    relativePath: String,
    isValueClass: Boolean,
    classSystem: ClassSystem
) {
  def className: String = fullyQualifiedName.split('.').last
  def fullPackage: String = fullyQualifiedName.stripSuffix(s".$className")
  def fullPackageAsPath: String = fullPackage.replace(".", "/")
  def folderPath: String = relativePath.stripSuffix(s"/$className.class")

  def readBytes(): Array[Byte] = classSystem.readBytes(relativePath)
}

private class ClassEntryLookUp(
    val entry: ClassEntry,
    fqcnToClassFile: Map[String, ClassFile],
    sourceUriToSourceFile: Map[SourceFileKey, SourceFile],
    sourceUriToClassFiles: Map[SourceFileKey, Seq[ClassFile]],
    classNameToSourceFile: Map[String, SourceFile],
    sourceNameToSourceFile: Map[String, Seq[SourceFile]],
    missingSourceFileClassFiles: Seq[ClassFile],
    val orphanClassFiles: Seq[ClassFile],
    logger: Logger
) {
  private val cachedSourceLines = mutable.Map[SourceLineKey, Seq[ClassFile]]()

  def sources: Iterable[SourceFileKey] = sourceUriToSourceFile.keys
  def fullyQualifiedNames: Iterable[String] = {
    classNameToSourceFile.keys ++
      orphanClassFiles.map(_.fullyQualifiedName) ++
      missingSourceFileClassFiles.map(_.fullyQualifiedName)
  }

  def uriFromFilename(filename: String): Option[URI] = {
    sourceNameToSourceFile.get(filename).flatMap(_.headOption).map(_.uri)
  }

  def classesByScalaName: Map[String, Iterable[String]] =
    fullyQualifiedNames.groupBy[String](NameTransformer.scalaClassName)

  def getFullyQualifiedClassName(
      sourceKey: SourceFileKey,
      lineNumber: Int
  ): Option[String] = {
    val line = SourceLineKey(sourceKey, lineNumber)

    if (!cachedSourceLines.contains(line)) {
      // read and cache line numbers from class files
      sourceUriToClassFiles
        .getOrElse(sourceKey, Nil)
        .groupBy(_.classSystem)
        .foreach { case (classSystem, classFiles) =>
          classSystem
            .within((_, root) => loadLineNumbers(root, classFiles, sourceKey))
            .warnFailure(logger, s"Cannot load line numbers in ${classSystem.name}")
        }
    }

    cachedSourceLines
      .get(line)
      .map { classFiles =>
        // The same breakpoint can stop in different classes
        // We choose the one with the smallest name
        classFiles.map(_.fullyQualifiedName).minBy(_.length)
      }
  }

  private def loadLineNumbers(
      root: Path,
      classFiles: Seq[ClassFile],
      sourceKey: SourceFileKey
  ): Unit = {
    for (classFile <- classFiles) {
      val path = root.resolve(classFile.relativePath)
      val inputStream = Files.newInputStream(path)
      try {
        val reader = new ClassReader(inputStream)

        val lineNumbers = mutable.Buffer[Int]()

        val visitor = new ClassVisitor(Opcodes.ASM9) {
          override def visitMethod(
              access: Int,
              name: String,
              desc: String,
              signature: String,
              exceptions: Array[String]
          ): MethodVisitor = {
            new MethodVisitor(Opcodes.ASM9) {
              override def visitLineNumber(line: Int, start: Label): Unit = {
                lineNumbers.append(line)
              }
            }
          }
        }
        reader.accept(visitor, 0)

        for (n <- lineNumbers) {
          val line = SourceLineKey(sourceKey, n)
          cachedSourceLines.update(
            line,
            cachedSourceLines.getOrElse(line, Seq.empty) :+ classFile
          )
        }
      } finally inputStream.close()
    }
  }

  def getSourceContent(sourceUri: URI): Option[String] =
    sourceUriToSourceFile.get(SourceFileKey(sourceUri)).flatMap(readSourceContent(_, logger))

  def getSourceFileURI(fqcn: String): Option[URI] =
    classNameToSourceFile.get(fqcn).map(_.uri)

  def getSourceContentFromClassName(fqcn: String): Option[String] =
    getSourceFileURI(fqcn).flatMap(getSourceContent)

  def getClassFiles(sourceUri: URI): Seq[ClassFile] =
    sourceUriToClassFiles.get(SourceFileKey(sourceUri)).getOrElse(Seq.empty)

  def getClassFile(fqcn: String): Option[ClassFile] =
    fqcnToClassFile.get(fqcn)

  private[internal] def getScalaSig(fqcn: String): Option[ScalaSig] = {
    def fromClass = for {
      classFile <- getClassFile(fqcn)
      if classFile.sourceName.exists(_.endsWith(".scala"))
      scalaSig <- Decompiler.decompile(classFile, logger)
    } yield scalaSig

    def fromSource = {
      val scalaSigs =
        for {
          sourceFile <- getSourceFileURI(fqcn).toSeq
          if sourceFile.toString.endsWith(".scala")
          classFile <- getClassFiles(sourceFile)
          if fqcn.startsWith(classFile.fullyQualifiedName + "$")
          scalaSig <- Decompiler.decompile(classFile, logger)
        } yield scalaSig
      if (scalaSigs.size > 1) throw Errors.frameDecodingFailure(s"more than one ScalaSig found for $fqcn")
      else scalaSigs.headOption
    }

    fromClass.orElse(fromSource)
  }
}

private object ClassEntryLookUp {
  private[internal] def apply(entry: ClassEntry, logger: Logger): ClassEntryLookUp = {
    val sourceLookUps = entry.sourceEntries.flatMap(SourceEntryLookUp(_, logger))
    ClassEntryLookUp(entry, sourceLookUps, logger)
  }

  def apply(
      entry: ClassEntry,
      sourceLookUps: Seq[SourceEntryLookUp],
      logger: Logger
  ): ClassEntryLookUp = {
    val classFiles = entry.classSystems.flatMap { classSystem =>
      classSystem
        .within(readAllClassFiles(classSystem))
        .warnFailure(logger, s"Cannot list the class files in ${classSystem.name}")
        .getOrElse(Vector.empty)
    }

    val classNameToClassFile =
      classFiles.map(c => (c.fullyQualifiedName, c)).toMap

    val sourceFileToRoot = sourceLookUps.flatMap(l => l.sourceFiles.map(f => (f -> l.root))).toMap
    val sourceUriToSourceFile = sourceLookUps.flatMap(_.sourceFiles).map(f => (SourceFileKey(f.uri), f)).toMap
    val sourceNameToSourceFile = sourceLookUps.flatMap(_.sourceFiles).groupBy(f => f.fileName)

    val classNameToSourceFile = mutable.Map[String, SourceFile]()
    val sourceUriToClassFiles = mutable.Map[SourceFileKey, Seq[ClassFile]]()
    val orphanClassFiles = mutable.Buffer[ClassFile]()
    val missingSourceFileClassFiles = mutable.Buffer[ClassFile]()

    for (classFile <- classFiles) {
      def recordSourceFile(sourceFile: SourceFile): Unit = {
        classNameToSourceFile.put(classFile.fullyQualifiedName, sourceFile)
        sourceUriToClassFiles.update(
          SourceFileKey(sourceFile.uri),
          sourceUriToClassFiles.getOrElse(
            SourceFileKey(sourceFile.uri),
            Seq.empty
          ) :+ classFile
        )
      }

      classFile.sourceName
        .flatMap(sourceNameToSourceFile.get)
        .getOrElse(Seq.empty)
        .toList match {
        case Nil =>
          // the source name is missing from the class file
          // or the source file is missing from the source entry
          missingSourceFileClassFiles.append(classFile)
        case sourceFile :: Nil =>
          // there is only one file with that name, it must be the right one
          // even if its relative path does not match the class package
          recordSourceFile(sourceFile)
        case manySourceFiles =>
          // there are several files with the same name
          // we find the one whose relative path matches the class package
          manySourceFiles.find(f => f.folderPath == classFile.folderPath) match {
            case Some(sourceFile) => recordSourceFile(sourceFile)
            case None =>
              // in some modules of the java 9+ runtimes, the pattern of the path
              // of the source files is <module>/<project>/src/<package>/<fileName>.java
              // we find the package name by splitting at "src/"
              manySourceFiles
                .filter(_.folderPath.contains("src/"))
                .find(f =>
                  f.folderPath
                    .split("src/")
                    .last == classFile.fullPackageAsPath
                ) match {
                case Some(sourceFile) => recordSourceFile(sourceFile)
                case None =>
                  // there is no source file with the correct relative path
                  // so we try to find the right package declaration in each file
                  // it would be very unfortunate that 2 sources file with the same name
                  // declare the same package.
                  manySourceFiles.filter { f =>
                    findPackage(f, sourceFileToRoot(f), classFile.fullPackage, logger)
                  } match {
                    case sourceFile :: Nil => recordSourceFile(sourceFile)
                    case _ => orphanClassFiles.append(classFile)
                  }
              }
          }
      }
    }

    if (orphanClassFiles.size > 0)
      logger.debug(s"Found ${orphanClassFiles.size} orphan class files in ${entry.name}")

    sourceLookUps.foreach(_.close())
    new ClassEntryLookUp(
      entry,
      classNameToClassFile,
      sourceUriToSourceFile,
      sourceUriToClassFiles.toMap,
      classNameToSourceFile.toMap,
      sourceNameToSourceFile,
      missingSourceFileClassFiles.toSeq,
      orphanClassFiles.toSeq,
      logger
    )
  }

  private def readAllClassFiles(
      classSystem: ClassSystem
  )(fileSystem: FileSystem, root: Path): Vector[ClassFile] = {
    if (Files.exists(root)) {
      val classMatcher = fileSystem.getPathMatcher("glob:**.class")
      Files
        .walk(root)
        .filter(classMatcher.matches)
        .iterator
        .asScala
        .map(readClassFile(classSystem, root))
        .toVector
    } else Vector.empty
  }

  private def readClassFile(classSystem: ClassSystem, root: Path)(
      path: Path
  ): ClassFile = {
    val inputStream = Files.newInputStream(path)
    var isValueClass = false
    try {
      val reader = new ClassReader(inputStream)
      val fullyQualifiedName = reader.getClassName.replace('/', '.')

      var sourceName = Option.empty[String]

      val visitor = new ClassVisitor(Opcodes.ASM9) {
        override def visitSource(source: String, debug: String): Unit =
          sourceName = Option(source)

        override def visitMethod(
            access: Int,
            name: String,
            descriptor: String,
            signature: String,
            exceptions: Array[String]
        ): MethodVisitor = {
          if (isStatic(access) && name.endsWith("$extension")) {
            isValueClass = true
          }
          super.visitMethod(access, name, descriptor, signature, exceptions)
        }

        private def isStatic(access: Int): Boolean = (access & Opcodes.ACC_STATIC) != 0
      }
      reader.accept(visitor, 0)
      val relativePath = root.relativize(path)
      ClassFile(
        fullyQualifiedName,
        sourceName,
        relativePath.toString,
        isValueClass,
        classSystem
      )
    } finally inputStream.close()
  }

  private def findPackage(sourceFile: SourceFile, root: Path, fullPackage: String, logger: Logger): Boolean = {
    // for "a.b.c" it returns Seq("a.b.c", "b.c", "c")
    // so that we can match on "package a.b.c" or "package b.c" or "package c"
    val nestedPackages = fullPackage.split('.').foldLeft(Seq.empty[String]) { (nestedParts, newPart) =>
      nestedParts.map(outer => s"$outer.$newPart") :+ newPart
    }
    val sourceContent = readSourceContent(sourceFile, root, logger)
    nestedPackages.exists { `package` =>
      val quotedPackage = Regex.quote(`package`)
      val matcher = s"package\\s+(object\\s+)?$quotedPackage(\\{|:|;|\\s+)".r
      sourceContent.exists(matcher.findFirstIn(_).isDefined)
    }
  }

  private def readSourceContent(sourceFile: SourceFile, logger: Logger): Option[String] = {
    withinSourceEntry(sourceFile.entry)(readSourceContent(sourceFile, _, logger))
      .warnFailure(logger, s"Cannot read content of ${sourceFile.uri}")
      .flatten
  }

  private def readSourceContent(sourceFile: SourceFile, root: Path, logger: Logger): Option[String] = {
    Try {
      val sourcePath = root.resolve(sourceFile.relativePath)
      new String(Files.readAllBytes(sourcePath))
    }
      .warnFailure(logger, s"Cannot read content of ${sourceFile.uri}")
  }

  private def withinSourceEntry[T](sourceEntry: SourceEntry)(f: Path => T): Try[T] = {
    sourceEntry match {
      case SourceJar(jar) => IO.withinJarFile(jar)(fs => f(fs.getPath("/")))
      case SourceDirectory(dir) => Try(f(dir))
      case StandaloneSourceFile(absolutePath, _) => Try(f(absolutePath.getParent))
    }
  }
}

/**
 * On a case-insensitive system we need to sanitize all URIs to use them as Map keys.
 */
private case class SourceFileKey(sanitizeUri: URI)

private object SourceFileKey {
  private val isCaseSensitiveFileSystem = Properties.isWin || Properties.isMac

  def apply(uri: URI): SourceFileKey = {
    val sanitizeUri: URI =
      if (isCaseSensitiveFileSystem) {
        uri.getScheme match {
          case "file" => URI.create(uri.toString.toUpperCase)
          case "jar" | "zip" if uri.toString.contains("!/") =>
            // The contents of jars are case-sensitive no matter what the filesystem is.
            val parts = uri.toString.split("!/", 2).toSeq
            val head = parts.head.toUpperCase()
            val tail = parts.tail.mkString("!/")
            URI.create(s"$head!/$tail")
          case _ => uri
        }
      } else uri
    new SourceFileKey(sanitizeUri)
  }
}

private case class SourceLineKey(sourceFile: SourceFileKey, lineNumber: Int)
