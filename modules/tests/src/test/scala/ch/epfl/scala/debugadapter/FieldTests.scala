package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.testfmk.*

class FieldTests extends DebugTestSuite {
  val scalaVersion = ScalaVersion.`3.1+`

  test("public and private fields") {
    val source =
      """|package example
         |
         |class A {
         |  var x: Int = 1
         |  var `val`: Int = 1
         |  private val y: String = "y"
         |  lazy val z: Int = 2
         |
         |  def foo: String = y
         |}
         |
         |object A {
         |  val z: Int = 2
         |  private var w: String = "w"
         |  private lazy val v: Int = 3
         |
         |  def bar: String = w + v
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    println(a.x)
         |    val a2 = A
         |    println(a2.z)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(23),
      LocalVariable("a")(Seq("val", "x", "y", "z")),
      Breakpoint(25),
      LocalVariable("a2")(Seq("w", "z", "v"))
    )
  }

  test("capture value class") {
    val source =
      """|package example
         |
         |class A(val x: Int) extends AnyVal:
         |  def foo =
         |    class B:
         |      def bar = x
         |    new B
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A(10)
         |    val b = a.foo
         |    println(b)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(13),
      LocalVariable("b")(Seq("<outer>", "x"))
    )
  }

  test("capture inline method") {
    val source =
      """|package example
         |
         |trait C
         |
         |object A:
         |  inline def withMode(inline op: C ?=> Object)(using C): Object = op
         |
         |  def foo(using C) = withMode {
         |    class B:
         |      def bar = summon[C]
         |    new B
         |  }
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = A
         |    val b = a.foo(using new C {})
         |    println(b)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(18),
      LocalVariable("b")(Seq("<outer>", "x$1$1"))
    )
  }

  test("anon lazy val") {
    val source =
      """|package example
         |
         |class A:
         |  lazy val (a, b) = (1, 2)
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    println(a)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(9),
      LocalVariable("a")(Seq("<anon>"))
    )
  }

  test("expanded names fields") {
    val source =
      """|package example
         |
         |trait A {
         |  def foo = 
         |    enum B:
         |      case C, D
         |    B
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A {}
         |    val b = a.foo
         |    println(b)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(14),
      LocalVariable("b")(Seq("<outer>", "$values", "C", "D", "B"))
    )
  }

  test("lazy ref") {
    val source =
      """|package example
         |trait C
         |
         |class A {
         |  def foo =
         |    lazy val c: C = new C {}
         |    class B:
         |      def ct = c
         |    new B
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    val b = a.foo
         |    println(b)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(16),
      LocalVariable("b")(Seq("<outer>", "c"))
    )
  }

  test("ambiguous indirect captures") {
    val source =
      """|package example
         |
         |class A():
         |  def bar =
         |    val x: Int = 12
         |    def getX = x
         |    def foo = 
         |      val x: Int = 1
         |      def met = x
         |      class B:
         |        def bar2 = met
         |        def bar3: Int = getX
         |      new B
         |    foo
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    val b = a.bar
         |    println(b)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(20),
      LocalVariable("b")(Seq("<outer>", "x$3", "x$4"))
    )
  }

  test("indirect capture") {
    val source =
      """|package example
         |
         |class A():
         |  def foo = 
         |    val x: Int = 1
         |    def met = x
         |    class B:
         |      def bar = met
         |    new B
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    val b = a.foo
         |    println(b)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(15),
      LocalVariable("b")(Seq("<outer>", "x"))
    )
  }

  test("anonymous using parameter") {
    val source =
      """|package example
         |
         |trait C
         |
         |class B (using C):
         |  def foo = summon[C]
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val b = new B(using new C {})
         |    println(b)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(11),
      LocalVariable("b")(Seq("x$1"))
    )
  }

  test("lazy val bitmap") {
    val source =
      """|package example
         |import scala.annotation.threadUnsafe
         |
         |class A:
         |  @threadUnsafe lazy val x: Int = 1
         |  @threadUnsafe lazy val y: Int = 1
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    println(a)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(9),
      LocalVariable("a")(Nil)
    )
  }

  test("class defined in a method fields") {
    val source =
      """|package example
         |
         |class Foo {
         |  def foo = 
         |    val x = " "
         |    class A:
         |      def bar = x
         |    new A
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val foo = new Foo
         |    val a = foo.foo
         |    println(a)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(15),
      LocalVariable("a")(Seq("x"))
    )
  }

  test("case field in JavaLangEnum") {
    val source =
      """|package example
         |
         |enum A extends java.lang.Enum[A] :
         |  case B
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = A.B
         |    println(a)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(9),
      LocalVariable("a")(
        Seq("name", "ordinal")
      ) // Seb m'a dit que fallait pas montrer dcp vu que B est static dans la classe A et non son companion Object
    )
  }

  test("serialVersionUID fields") {
    val source =
      """|package example
         |
         |@SerialVersionUID(1L)
         |class A
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    println(a)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(8),
      LocalVariable("a")(Nil)
    )
  }

  test("static fields in static classes Java") {
    val source =
      """|package example;
         |
         |final class A {
         |  public static final int x = 1;
         |}
         |
         |public class Main {
         |  public static void main(String[] args) {
         |    A a = new A();
         |    System.out.println(a.x);
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.fromJavaSource(source, "example.Main", scalaVersion)
    check(
      Breakpoint(10),
      LocalVariable("a")(Seq("Class has no fields"))
    )
  }

  test("extend trait with given fields") {
    val source =
      """|package example
         |
         |trait A:
         |  given x: Int = 1
         |
         |class C extends A
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val c = new C
         |    println(c)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(11),
      LocalVariable("c")(Seq("x"))
    )
  }

  test("extend traits with val fields") {
    val source =
      """|package example
         |
         |trait A {
         |  private val x: Int = 1
         |  private val y: Int = 2
         |  val z: Int = 3
         |}
         |
         |class B extends A {
         |  val y: Int = 2
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val b = new B
         |    println(b)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(16),
      LocalVariable("b")(Seq("x", "example$A$$y", "y", "z"))
    )
  }

  test("notFound offset_m field") {
    val source =
      """|package example
         |
         |trait A {
         |  def foo: Int
         |}
         |class C:
         |  object B extends A {
         |    lazy val foo: Int = 42
         |  }
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val b = (new C).B
         |    println(b)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(14),
      LocalVariable("b")(Seq("foo"))
    )
  }

  test("ambiguous Object/ImplicitClass fields") {
    val source =
      """|package example
         |
         |object A {
         |  object B
         |
         |  implicit class B (val x: Int)
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = A
         |    println(a)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(12),
      LocalVariable("a")(Seq("B"))
    )
  }

  test("public and private objects") {
    val source =
      """|package example
         |
         |class A {
         |  object B
         |  private object C
         |}
         |
         |object A {
         |  object D
         |  private object E
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    println(a)
         |    val a2 = A
         |    println(a2)
         |    val d = A.D
         |    println(d)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(16),
      LocalVariable("a")(Seq("B", "C")),
      Breakpoint(18),
      LocalVariable("a2")(Seq("D", "E")),
      Breakpoint(20),
      LocalVariable("d")(Seq("Class has no fields"))
    )
  }

  test("outer field") {
    val source =
      """|package example
         |
         |class A[T](x: T){
         |  class B {
         |    def foo: T = x
         |  }
         |
         |  def bar: (T, Object) = {
         |    class C {
         |      def foo: T = x
         |    }
         |    ((new C).foo, new C)
         |  }
         |
         |  def newB: B = new B
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A(10)
         |    val b = a.newB
         |    val c = a.bar._2
         |    println(b)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(23),
      LocalVariable("a")(Seq("x")),
      LocalVariable("b")(Seq("<outer>")),
      LocalVariable("c")(Seq("<outer>"))
    )
  }

  test("intricated outer fields") {
    val source =
      """|package example
         |
         |trait A {
         |  class X
         |  def foo = new X
         |}
         |
         |trait B extends A {
         |  class Y {
         |    class Z extends X
         |    def newZ = new Z
         |  }
         |  def newY = new Y
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val x = (new A {}).foo
         |    println(x)
         |    val z = (new B {}).newY.newZ
         |    println(z)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(19),
      LocalVariable("x")(Seq("<outer>")),
      Breakpoint(21),
      LocalVariable("z")(Seq("<outer>", "<outer>")) // we have 2 outer??
    )
  }
}
