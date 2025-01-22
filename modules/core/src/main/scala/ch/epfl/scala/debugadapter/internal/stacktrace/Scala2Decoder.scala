package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.internal.ByteCode
import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider
import ch.epfl.scala.debugadapter.internal.ThrowOrWarn
import ch.epfl.scala.debugadapter.internal.scalasig.ScalaSigPrinter
import ch.epfl.scala.debugadapter.internal.scalasig.*
import ch.epfl.scala.debugadapter.internal.stacktrace.JdiExtensions.*
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedMethod
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedVariable
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedField

import com.sun.jdi

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal
import scala.util.matching.Regex

class Scala2Decoder(
    sourceLookUp: SourceLookUpProvider,
    scalaVersion: ScalaVersion,
    protected val logger: Logger,
    protected val testMode: Boolean
) extends ScalaDecoder
    with ThrowOrWarn {

  override def reload(): Unit = ()

  override def skipOver(method: jdi.Method): Boolean =
    try
      if (method.isBridge) true
      else if (method.declaringType.isDynamicClass) true
      else if (method.isJava) false
      else if (method.isConstructor) false
      else if (method.isStaticConstructor) false
      else if (method.isAdaptedMethod) true
      else if (method.isAnonFunction) false
      else if (method.isPrivateAccessor) true
      else if (method.isLiftedMethod) !method.isLazyInitializer && method.isLazyGetter
      else if (method.declaringType.isAnonClass) false
      else if (method.declaringType.isLocalClass) false
      else if (method.declaringType.isNestedClass) false
      else if (method.isDefaultValue) false
      else if (method.isTraitInitializer) method.bytecodes.toSeq == Seq(ByteCode.RETURN)
      else if (method.isLazyInitializer) skipLazyInitializer(method)
      else if (method.isStaticMain) false
      else {
        val fqcn = method.declaringType.name
        val isObject = fqcn.endsWith("$")
        sourceLookUp.getScalaSig(fqcn) match {
          case None =>
            throwOrWarn(s"Cannot find Pickle for $fqcn")
            false
          case Some(sig) =>
            val matchingMethods = sig.entries.toSeq
              .collect { case m: MethodSymbol if m.isMethod => m }
              .filter(_.parent.exists(_.isModule == isObject))
              .filter(matchSymbol(method, _))

            if (matchingMethods.size > 1) {
              val builder = new java.lang.StringBuilder
              builder.append(
                s"Found ${matchingMethods.size} matching symbols for $method:\n"
              )
              val printer = new ScalaSigPrinter(builder)
              matchingMethods.foreach(printer.printSymbol)
              throwOrWarn(builder.toString)
            }

            matchingMethods.forall(skip)
        }
      }
    catch {
      case NonFatal(e) => throwOrWarn(e); false
    }

  override def decode(method: jdi.Method): DecodedMethod =
    JavaMethod(method, isGenerated = skipOver(method))

  override def decode(variable: jdi.LocalVariable, method: jdi.Method, sourceLine: Int): DecodedVariable =
    JavaVariable(variable)

  override def decode(field: jdi.Field): DecodedField =
    JavaField(field)

  private def containsLazyField(interface: jdi.InterfaceType, fieldName: String): Boolean = {
    val fqcn = interface.name
    sourceLookUp.getScalaSig(fqcn).exists(containsLazyField(_, fieldName))
  }

  private def skipLazyInitializer(method: jdi.Method): Boolean = {
    val fieldName = method.name.dropRight(11)
    method.declaringType match {
      case cls: jdi.ClassType =>
        cls.allInterfaces.asScala.exists(interface => containsLazyField(interface, fieldName))
      case t =>
        val message =
          s"Expected declaring type of $method to be a class, found ${t.getClass.getSimpleName}"
        throwOrWarn(message)
        false
    }
  }

  private def containsLazyField(scalaSig: ScalaSig, fieldName: String): Boolean = {
    scalaSig.entries.exists {
      case m: MethodSymbol => m.isLazy && m.name == fieldName
      case _ => false
    }
  }

  private def skip(scalaMethod: MethodSymbol): Boolean = {
    scalaMethod.isSynthetic ||
    // we skip if it is an accessor, except if it is the accessor of a lazy field in a trait
    // because the accessor of a lazy field in a trait is its initializer
    (scalaMethod.isAccessor && (!scalaMethod.isLazy || !scalaMethod.parent.get.isTrait))
  }

  private def matchSymbol(javaMethod: jdi.Method, scalaMethod: MethodSymbol): Boolean = {
    // TODO find what is an aliasRef
    if (scalaMethod.aliasRef.nonEmpty)
      logger.debug(
        s"aliasRef for ${scalaMethod.name}: ${scalaMethod.aliasRef}"
      )
    matchName(javaMethod, scalaMethod) &&
    matchOwner(javaMethod.declaringType, scalaMethod.parent.get) &&
    matchSignature(javaMethod, scalaMethod.infoType)
  }

  private def matchName(javaMethod: jdi.Method, scalaMethod: MethodSymbol): Boolean = {
    val javaName = javaMethod.name
    val javaPrefix = javaMethod.declaringType.name.replace('.', '$') + "$$"
    // if an inner accesses a private method, the backend makes the method public
    // and prefixes its name with the full class name.
    // Example: method foo in class example.Inner becomes example$Inner$$foo
    val expectedName = javaName.stripPrefix(javaPrefix)
    scalaMethod.name == expectedName
  }

  private def matchOwner(javaClass: jdi.ReferenceType, scalaClass: Symbol): Boolean = {
    // TODO try use tryEncode
    getOwners(scalaClass)
      .foldRight(Option(javaClass.name)) { (sym, acc) =>
        for (javaName <- acc if javaName.contains(sym.name))
          yield javaName
            .split(Regex.quote(sym.name))
            .drop(1)
            .mkString(sym.name)
      }
      .exists { remainder =>
        remainder.forall(c => c.isDigit || c == '$')
      }
  }

  private def getOwners(sym: Symbol): Seq[Symbol] =
    Iterator
      .iterate(Option(sym))(opt => opt.flatMap(_.parent))
      .takeWhile(_.isDefined)
      .flatten
      .filter(_.name != "<empty>") // Top level classes have <empty> packag as owner
      .toSeq

  private def matchSignature(javaMethod: jdi.Method, methodType: Type): Boolean = {
    val (scalaArgs, scalaReturnType) = extractParametersAndReturnType(methodType)
    def matchAllArguments: Boolean = {
      val javaArgs = javaMethod.arguments.asScala.toSeq
      javaArgs.corresponds(scalaArgs) { (javaArg, scalaArg) =>
        matchArgument(javaArg, scalaArg, javaMethod.declaringType)
      }
    }
    def matchReturnType: Boolean = {
      try matchType(javaMethod.returnType, scalaReturnType, javaMethod.declaringType)
      catch {
        // javaMethod.returnType can throw ClassNotLoadedException
        case cause: jdi.ClassNotLoadedException => true
      }
    }
    matchAllArguments && matchReturnType
  }

  private[internal] def extractParametersAndReturnType(methodType: Type): (Seq[Symbol], Type) =
    methodType match {
      case m: FunctionType =>
        val (params, returnType) = extractParametersAndReturnType(
          m.resultType.get
        )
        (m.paramSymbols ++ params, returnType)
      case m: NullaryMethodType => (Seq.empty, m.resultType.get)
      case m: PolyType => extractParametersAndReturnType(m.typeRef.get)
      case returnType => (Seq.empty, returnType)
    }

  private def matchArgument(javaArg: jdi.LocalVariable, scalaArg: Symbol, declaringType: jdi.Type): Boolean = {
    val scalaType = scalaArg.asInstanceOf[MethodSymbol].infoType
    javaArg.name == scalaArg.name && (
      // we cannot check the type of the `this` argument in methods of value classes
      scalaArg.name == "$this" || matchType(javaArg.`type`, scalaType, declaringType)
    )
  }

  private def matchType(javaType: jdi.Type, scalaType: Type, declaringType: jdi.Type): Boolean = {
    scalaType match {
      case ThisType(sym) => javaType == declaringType
      case SingleType(typeRef, sym) =>
        matchTypeSymbol(javaType, sym, declaringType)
      case ConstantType(constant) =>
        val expectedType = constant.get.value match {
          case name: Ref[_] => "java.lang.String"
          case value =>
            value.getClass.getName match {
              case "java.lang.Boolean" => "boolean"
              case "java.lang.Byte" => "byte"
              case "java.lang.Character" => "char"
              case "java.lang.Double" => "double"
              case "java.lang.Float" => "float"
              case "java.lang.Integer" => "int"
              case "java.lang.Long" => "long"
              case "java.lang.Short" => "short"
            }
        }
        javaType.name == expectedType
      case TypeRefType(prefix, sym, typeArgs) =>
        matchTypeSymbol(javaType, sym, declaringType)
      case RefinedType(sym, typeRefs) =>
        typeRefs.exists(matchType(javaType, _, declaringType))
      case AnnotatedType(typeRef) =>
        matchType(javaType, typeRef.get, declaringType)
      case ExistentialType(typeRef, paramRefs) =>
        matchType(javaType, typeRef.get, declaringType)
      case PolyType(typeRef, paramRefs) =>
        val upperBound = extractUpperBound(typeRef.get)
        matchType(javaType, upperBound, declaringType)
      case other =>
        throwOrWarn(s"Unexpected type found: $other")
        true
    }
  }

  private val scalaToJavaPrimitiveTypes: Map[String, String] = Map(
    "scala.Unit" -> "void",
    "scala.Boolean" -> "boolean",
    "scala.Char" -> "char",
    "scala.Byte" -> "byte",
    "scala.Short" -> "short",
    "scala.Int" -> "int",
    "scala.Long" -> "long",
    "scala.Float" -> "float",
    "scala.Double" -> "double"
  )

  private[internal] val scalaAliasesToJavaTypes = {
    Map(
      "scala.Nothing" -> "scala.runtime.Nothing$",
      "scala.Null" -> "scala.runtime.Null$",
      "scala.Any" -> "java.lang.Object",
      "scala.AnyRef" -> "java.lang.Object",
      "scala.Predef.String" -> "java.lang.String",
      "scala.Predef.Class" -> "java.lang.Class",
      "scala.Predef.Function" -> "scala.Function1",
      "scala.Predef.Map" -> "scala.collection.immutable.Map",
      "scala.Predef.Set" -> "scala.collection.immutable.Set",
      "scala.Predef.OptManifest" -> "scala.reflect.OptManifest",
      "scala.Predef.Manifest" -> "scala.reflect.Manifest",
      "scala.Predef.Pair" -> "scala.Tuple2",
      "scala.Predef.Triple" -> "scala.Tuple3",
      "scala.package.Throwable" -> "java.lang.Throwable",
      "scala.package.Exception" -> "java.lang.Exception",
      "scala.package.Error" -> "java.lang.Error",
      "scala.package.RuntimeException" -> "java.lang.RuntimeException",
      "scala.package.NullPointerException" -> "java.lang.NullPointerException",
      "scala.package.ClassCastException " -> "java.lang.ClassCastException",
      "scala.package.IndexOutOfBoundsException" -> "java.lang.IndexOutOfBoundsException",
      "scala.package.ArrayIndexOutOfBoundsException" -> "java.lang.ArrayIndexOutOfBoundsException",
      "scala.package.StringIndexOutOfBoundsException" -> "java.lang.StringIndexOutOfBoundsException",
      "scala.package.UnsupportedOperationException" -> "java.lang.UnsupportedOperationException",
      "scala.package.IllegalArgumentException" -> "java.lang.IllegalArgumentException",
      "scala.package.NoSuchElementException" -> "java.util.NoSuchElementException",
      "scala.package.NumberFormatException" -> "java.lang.NumberFormatException",
      "scala.package.AbstractMethodError" -> "java.lang.AbstractMethodError",
      "scala.package.InterruptedException" -> "java.lang.InterruptedException",
      "scala.package.Iterable" -> "scala.collection.Iterable",
      "scala.package.Seq" -> "scala.collection.Seq",
      "scala.package.IndexedSeq" -> "scala.collection.IndexedSeq",
      "scala.package.Iterator" -> "scala.collection.Iterator",
      "scala.package.BufferedIterator" -> "scala.collection.BufferedIterator",
      "scala.package.List" -> "scala.collection.immutable.List",
      "scala.package.::" -> "scala.collection.immutable.$colon$colon",
      "scala.package.Stream" -> "scala.collection.immutable.Stream",
      "scala.package.Vector" -> "scala.collection.immutable.Vector",
      "scala.package.StringBuilder" -> "scala.collection.mutable.StringBuilder",
      "scala.package.Range" -> "scala.collection.immutable.Range",
      "scala.package.BigDecimal" -> "scala.math.BigDecimal",
      "scala.package.BigInt" -> "scala.math.BigInt",
      "scala.package.Equiv" -> "scala.math.Equiv",
      "scala.package.Fractional" -> "scala.math.Fractional",
      "scala.package.Integral" -> "scala.math.Integral",
      "scala.package.Numeric" -> "scala.math.Numeric",
      "scala.package.Ordered" -> "scala.math.Ordered",
      "scala.package.Ordering" -> "scala.math.Ordering",
      "scala.package.PartialOrdering" -> "scala.math.PartialOrdering",
      "scala.package.PartiallyOrdered" -> "scala.math.PartiallyOrdered",
      "scala.package.Either" -> "scala.util.Either",
      "scala.package.Left" -> "scala.util.Left",
      "scala.package.Right" -> "scala.util.Right",
      "scala.<byname>" -> "scala.Function0"
    ) ++ (
      if (scalaVersion.isScala212)
        Map(
          "scala.Predef.ClassManifest" -> "scala.reflect.ClassTag",
          "scala.package.TraversableOnce" -> "scala.collection.TraversableOnce",
          "scala.package.Traversable" -> "scala.collection.Traversable",
          "scala.<repeated>" -> "scala.collection.Seq"
        )
      else
        Map(
          "scala.package.Cloneable" -> "java.lang.Cloneable",
          "scala.package.Seriazable" -> "java.io.Serializable",
          "scala.package.TraversableOnce" -> "scala.collection.IterableOnce",
          "scala.package.IterableOnce" -> "scala.collection.IterableOnce",
          "scala.package.Traversable" -> "scala.collection.Iterable",
          "scala.package.LazyList" -> "scala.collection.immutable.LazyList",
          "scala.<repeated>" -> "scala.collection.immutable.Seq"
        )
    )
  }

  private[internal] val scalaToJavaTypes =
    scalaToJavaPrimitiveTypes ++ scalaAliasesToJavaTypes

  private def matchTypeSymbol(javaType: jdi.Type, sym: Symbol, declaringType: jdi.Type): Boolean = {
    val encoded = tryEncodeType(sym)
    val path = sym.path
    sym match {
      case TypeSymbol(info) =>
        val upperBound = extractUpperBound(info.info.get)
        matchType(javaType, upperBound, declaringType)
      case AliasSymbol(info) =>
        val tpe = info.info.get
        matchType(javaType, tpe, declaringType)
      case _ =>
        if (path == "scala.Array") {
          // TODO compare type parameter
          // if the type parameter is an unbounded T then the erasure is java.lang.Object
          javaType.isInstanceOf[jdi.ArrayType] ||
          javaType.name == "java.lang.Object"
        } else if (scalaToJavaTypes.contains(path)) {
          // sym is a primitive or a type alias from the Scala library
          javaType.name == scalaToJavaTypes(path)
        } else {
          lazy val ifEmpty = {
            throwOrWarn(s"Empty encoded value for $path")
            // if the symbol cannot be encoded, we return true
            // because we don't want to skip a method that should not be skipped
            true
          }
          encoded.fold(ifEmpty) { encodedName =>
            // should not fail because encodedName check that it exists
            val classFile = sourceLookUp.getClassFile(encodedName).get
            classFile.isValueClass || encodedName == javaType.name
          }
        }
    }
  }

  private def extractUpperBound(tpe: Type): Type = {
    tpe match {
      case TypeBoundsType(_, upper) => upper.get
      case other => other
    }
  }

  private def tryEncodeType(sym: Symbol): Option[String] = {
    def encodePrefix(sym: Symbol): Option[String] = {
      sym match {
        case NoSymbol => Some("")
        case _: ExternalSymbol =>
          val prefix =
            sym.parent.fold(Option(""))(encodePrefix).map(_ + sym.name)
          prefix.map { prefix =>
            // if the encoded name of the symbol is not known by the sourceLookUp, we assume it is a package
            if (sourceLookUp.containsClass(prefix)) prefix + "$"
            else prefix + "."
          }
        case _ =>
          val prefix = sym.parent.flatMap(encodePrefix).getOrElse("") + sym.name + "$"
          Some(prefix).filter(sourceLookUp.containsClass)
      }
    }
    val encoded = sym match {
      case obj: ObjectSymbol =>
        obj.parent.fold(Option(""))(encodePrefix).map(_ + obj.name + "$")
      case _ => sym.parent.fold(Option(""))(encodePrefix).map(_ + sym.name)
    }
    encoded.filter(sourceLookUp.containsClass)
  }
}
