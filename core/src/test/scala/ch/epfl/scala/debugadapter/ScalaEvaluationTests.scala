package ch.epfl.scala.debugadapter

import utest._

object Scala212EvaluationTests extends ScalaEvaluationTests(ScalaVersion.`2.12`)
object Scala213EvaluationTests extends ScalaEvaluationTests(ScalaVersion.`2.13`)
object Scala30EvaluationTests extends ScalaEvaluationTests(ScalaVersion.`3.0`)
object Scala31EvaluationTests extends ScalaEvaluationTests(ScalaVersion.`3.1`)

abstract class ScalaEvaluationTests(scalaVersion: ScalaVersion)
    extends ScalaEvaluationSuite(scalaVersion) {

  def tests: Tests = Tests {
    "evaluate local variables" - {
      val source =
        """|package example
           |object App {
           |  def main(args: Array[String]): Unit = {
           |    val str = "hello"
           |    val x1 = 1
           |    println("Hello, World!")
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.App")(
        Breakpoint(6)(
          Evaluation.success("x1 + 2", 3),
          Evaluation.success("str.reverse", "olleh")
        )
      )
    }

    "evaluate public and private fields in object" - {
      val source =
        """|package example
           |
           |object A {
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |
           |  val a1 = "a1"
           |  private val a2 = "a2"
           |  private[this] val a3 = "a3"
           |  private[example] val a4 = "a4"
           |
           |  override def toString: String =
           |    a2 + a3
           |
           |  object B {
           |    val b1 = "b1"
           |    private val b2 = "b2"
           |    private[A] val b3 = "b3"
           |    private[example] val b4 = "b4"
           |  }
           |
           |  private object C
           |  private[this] object D
           |  private[example] object E
           |}
           |
           |object F {
           |  val f1 = "f1"
           |  private[example] val f2 = "f2"
           |
           |  object G
           |  private[example] object H
           |}
           |""".stripMargin
      assertInMainClass(source, "example.A")(
        Breakpoint(5)(
          Evaluation.success("a1", "a1"),
          Evaluation.success("this.a1", "a1"),
          Evaluation.success("A.this.a1", "a1"),
          Evaluation.success("a2", "a2"),
          Evaluation.successOrIgnore("a3", "a3", ignore = isScala2),
          Evaluation.success("a4", "a4"),
          Evaluation.success("B.b1", "b1"),
          Evaluation.success("this.B.b1", "b1"),
          Evaluation.success("A.B.b1", "b1"),
          Evaluation.success("A.this.B.b1", "b1"),
          Evaluation.failed("B.b2")(_ => true),
          Evaluation.success("B.b3", "b3"),
          Evaluation.success("A.B.b3", "b3"),
          Evaluation.success("B.b4", "b4"),
          Evaluation.success("C")(_.startsWith("A$C$@")),
          Evaluation.success("D")(_.startsWith("A$D$@")),
          Evaluation.success("F.f1", "f1"),
          Evaluation.success("F.f2", "f2"),
          Evaluation.success("F.G")(_.startsWith("F$G$@")),
          Evaluation.success("F.H")(_.startsWith("F$H$@"))
        )
      )
    }

    "evaluate public and private methods in static object" - {
      val source =
        """|package example
           |
           |object A {
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |
           |  def a1(str: String) = s"a1: $str"
           |  private def a2(str: String) = s"a2: $str"
           |  
           |  private object B {
           |    def b1(str: String) = s"b1: $str"
           |    private[A] def b2(str: String) = s"b2: $str"
           |  }
           |}
           |
           |object C {
           |  def c1(str: String) = s"c1: $str"
           |  private def c2(str: String) = s"c2: $str"
           |}
        """.stripMargin
      assertInMainClass(source, "example.A")(
        Breakpoint(5)(
          Evaluation.success("a1(\"foo\")", "a1: foo"),
          Evaluation.success("a2(\"foo\")", "a2: foo"),
          Evaluation.success("B.b1(\"foo\")", "b1: foo"),
          Evaluation.success("B.b2(\"foo\")", "b2: foo"),
          Evaluation.success("C.c1(\"foo\")", "c1: foo"),
          Evaluation.failed("C.c2(\"foo\")")(_ => true)
        )
      )
    }

    "evaluate in private static object" - {
      val source =
        """|package example
           |
           |object A {
           |  private val a1 = "a1"
           |  private def a2(str: String): String = {
           |    s"a2: $str"
           |  }
           |  override def toString(): String = a1
           |  
           |  private object B {
           |    val b1 = "b1"
           |    def b2(str: String): String = {
           |      s"b2: $str"
           |    }
           |  }
           |
           |  def main(args: Array[String]): Unit = {
           |    println(B.b2("foo"))
           |  }
           |}
           |
           |object C {
           |  def c1(str: String) = s"c1: $str"
           |  private def c2(str: String) = s"c2: $str"
           |}
        """.stripMargin
      assertInMainClass(source, "example.A")(
        Breakpoint(13)(
          Evaluation.success("b1", "b1"),
          Evaluation.success("b2(\"foo\")", "b2: foo"),
          Evaluation.successOrIgnore("a1", "a1", ignore = isScala2),
          Evaluation.successOrIgnore(
            "a2(\"foo\")",
            "a2: foo",
            ignore = isScala2
          )
        )
      )
    }

    "evaluate public and private fields in class" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val a = new A("a")
           |    println(a)
           |  }
           |}
           |
           |class A(name: String) {
           |  val a1 = s"$name.a1"
           |  private val a2 = s"$name.a2"
           |  
           |  object B {
           |    val  b1 = s"$name.B.b1"
           |  }
           |
           |  private object C  {
           |    val c1 = s"$name.C.c1"
           |  }
           |
           |  override def toString: String = {
           |    name + a2
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(6)(
          Evaluation.success("a.a1", "a.a1"),
          Evaluation.success("a.B.b1", "a.B.b1"),
          Evaluation.success("new A(\"aa\").a1", "aa.a1"),
          Evaluation.success("new A(\"aa\").B.b1", "aa.B.b1")
        ),
        Breakpoint(23)(
          Evaluation.success("name", "a"),
          Evaluation.success("this.name", "a"),
          Evaluation.success("a1", "a.a1"),
          Evaluation.success("a2", "a.a2"),
          Evaluation.successOrIgnore("new A(\"aa\").a2", "aa.a2", isScala2),
          Evaluation.success("B.b1", "a.B.b1"),
          Evaluation.success("this.B.b1", "a.B.b1"),
          Evaluation.success("C.c1", "a.C.c1"),
          Evaluation.success("new A(\"aa\").C.c1", "aa.C.c1")
        )
      )
    }

    "evaluate private method call in class" - {
      val source =
        """|package example
           |
           |class A {
           |  val a = this
           |
           |  def foo(): String = {
           |    m("foo") // breakpoint
           |  }
           |
           |  private def m(x: String) = x
           |}
           |
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    new A().foo()
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(7)(
          Evaluation.success("m(\"foo\")", "foo"),
          Evaluation.success("this.m(\"bar\")", "bar"),
          Evaluation.success("a.m(\"fizz\")", "fizz")
        )
      )
    }

    "evaluate private overloaded method" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |  
           |  trait A
           |  class B extends A
           |  
           |  private def m(): String = "m"
           |  private def m(n: Int): String = s"m($n: Int)"
           |  private def m(b: Boolean): String = s"m($b: Boolean)"
           |  private def m(str: String): String = s"m($str: String)"
           |  private def m(a: A): String = s"m(a: A)"
           |  private def m(b: B): String = s"m(b: B)"
           |  private def m(xs: Array[Int]): String = s"m(xs: Array[Int])"
           |  private def m(xs: Array[A]): String = s"m(xs: Array[A])"
           |  private def m(xs: Array[Array[Int]]): String = s"m(xs: Array[Array[Int]])"
           |
           |  private def m1(xs: Seq[Int]): String = xs.toString
           |  private def m1(xs: Seq[Boolean]): Int = xs.count(identity)
           |}
           |
           |
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(5)(
          Evaluation.success("m()", "m"),
          Evaluation.success("m(5)", "m(5: Int)"),
          Evaluation.success("m(true)", "m(true: Boolean)"),
          Evaluation.success("m(\"foo\")", "m(foo: String)"),
          Evaluation.successOrIgnore("m(new B)", "m(b: B)", isScala2),
          Evaluation.successOrIgnore("m(new B: A)", "m(a: A)", isScala2),
          Evaluation
            .successOrIgnore("m(Array(1, 2))", "m(xs: Array[Int])", isScala2),
          Evaluation
            .successOrIgnore("m(Array[A](new B))", "m(xs: Array[A])", isScala2),
          Evaluation.successOrIgnore(
            "m(Array(Array(1), Array(2)))",
            "m(xs: Array[Array[Int]])",
            isScala2
          ),
          Evaluation
            .successOrIgnore("m1(Seq(1, 2, 3))", "List(1, 2, 3)", isScala2),
          Evaluation.successOrIgnore(
            "m1(Vector(1, 2, 3))",
            "Vector(1, 2, 3)",
            isScala2
          ),
          Evaluation.successOrIgnore("m1(Seq(true, false, true))", 2, isScala2)
        )
      )
    }

    "evaluate private inner class" - {
      val source =
        """|package example
           |
           |object A {
           |  def main(args: Array[String]): Unit = {
           |    val c = new C
           |    c.c1()
           |  }
           |
           |  private def a1(): B = new B
           |  private def a2(b: B): String = "a2"
           |
           |  private class B {
           |    val b1: String = "b1"
           |    def b2(): String = "b2"
           |  }
           |}
           |
           |class C {
           |  def c1(): Unit =
           |    println("Hello, World!")
           |  
           |  private def c2(): D = new D
           |  private def c3(d: D): String = "c3"
           |
           |  private class D {
           |    val d1: String = "d1"
           |    def d2(): String = "d2"
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.A")(
        Breakpoint(5)(
          Evaluation.success("a1()")(_.startsWith("A$B@")),
          Evaluation.success("(new B).b1", "b1"),
          Evaluation.success("(new A.B).b2()", "b2"),
          Evaluation.success("a2(new B)", "a2")
        ),
        Breakpoint(20)(
          Evaluation.success("c2()")(_.startsWith("C$D@")),
          Evaluation.success("(new D).d1", "d1"),
          Evaluation.success("(new this.D).d2()", "d2"),
          Evaluation.success("c3(new D)", "c3")
        )
      )
    }

    "evaluate constructor of inner class (with captured outer)" - {
      val source =
        """|package example
           |
           |object A {
           |  def main(args: Array[String]): Unit = {
           |    val b = new B
           |    b.m()
           |  }
           |}
           |
           |class B {
           |  class C
           |  def m(): Unit = {
           |    println("m")
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.A")(
        Breakpoint(6)(
          Evaluation.success("new b.C")(_.startsWith("B$C@"))
        ),
        Breakpoint(13)(
          Evaluation.success("new C")(_.startsWith("B$C@"))
        )
      )
    }

    "evaluate shaded fields and values" - {
      val source =
        """|package example
           |
           |class A {
           |  val x1 = "ax1"
           |  val x2 = "ax2"
           |  class B {
           |    val x2 = "bx2"
           |    val x3 = "bx3"
           |    def m1(): Unit = {
           |      val x3 = "x3"
           |      println(x1 + x2 + x3)
           |    }
           |  }
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val a = new A()
           |    val b = new a.B()
           |    b.m1()
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(11)(
          Evaluation.success("x1 + x2 + x3", "ax1bx2x3"),
          Evaluation.successOrIgnore(
            "x1 + A.this.x2 + this.x3",
            "ax1ax2bx3",
            isScala2
          )
        )
      )
    }

    "evaluate field of two-level deep outer class" - {
      val source =
        """|package example
           |
           |class A {
           |  private val a = "a"
           |  class B {
           |    class C {
           |      def m(): Unit = {
           |        println(a)
           |      }
           |    }
           |  }
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val a = new A
           |    val b = new a.B
           |    val c = new b.C
           |    println(c.m())
           |  }
           |}
           |
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(8)(Evaluation.success("a", "a"))
      )
    }

    "fail evaluation of the outer class of a private final class" - {
      val source =
        """|package example
           |
           |class A {
           |  val a1 = "a1"
           |  private final class B {
           |    def b1(): Unit = {
           |      println("b1")
           |    }
           |  }
           |  def a2(): Unit = {
           |    val b = new B
           |    b.b1()
           |  }
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val a = new A
           |    a.a2()
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(7)(
          Evaluation.success("a1", new NoSuchFieldException("$outer"))
        )
      )
    }

    "evaluate from an local class" - {
      val source =
        """|package example
           |
           |class A {
           |  val x1 = "ax1"
           |  def m(): Unit = {
           |    val x1 = "x1"
           |    class B {
           |      val x2 = "bx2"
           |      def m(): Unit = {
           |        val x2 = "x2"
           |        println(x1 + A.this.x1)
           |      }
           |    }
           |    val b = new B
           |    b.m()
           |  }
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val a = new A
           |    a.m()
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(11)(
          // B captures the local value x1
          Evaluation.successOrIgnore("new B", isScala2)(_.startsWith("A$B$1@")),
          // x1 is captured by B
          Evaluation.successOrIgnore("x1", "x1", isScala2),
          Evaluation.success("x2", "x2"),
          Evaluation.successOrIgnore("A.this.x1", "ax1", isScala2),
          Evaluation.successOrIgnore("this.x2", "bx2", isScala2)
        )
      )
    }

    "evaluate nested methods" - {
      val source =
        """|package example
           |
           |object A {
           |  private class B {
           |    override def toString(): String = "b"
           |  }
           |  def main(args: Array[String]): Unit = {
           |    val x1 = 1
           |    def m1(name: String): String = {
           |      s"m$x1($name)"
           |    }
           |    def m2(b: B): String = {
           |      s"m2($b)"
           |    }
           |    def m3(): B = {
           |      new B
           |    }
           |    println(m1("m") + m2(m3()))
           |    val c = new C
           |    c.m()
           |  }
           |}
           |
           |class C {
           |  val x1 = 1
           |  private class D {
           |    override def toString(): String = "d"
           |  }
           |  def m(): Unit = {
           |    def m1(name: String): String = {
           |      s"m$x1($name)"
           |    }
           |    def m2(d: D): String = {
           |      s"m2($d)"
           |    }
           |    def m3(): D = {
           |      new D
           |    }
           |    println(m1("m") + m2(m3()))
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.A")(
        Breakpoint(18)(
          Evaluation.success("m1(\"x\")", "m1(x)"),
          Evaluation.success("m3()")(_.startsWith("A$B@")),
          Evaluation.success("m2(new B)", "m2(b)")
        ),
        Breakpoint(39)(
          Evaluation.success("m1(\"x\")", "m1(x)"),
          Evaluation.success("m3()")(_.startsWith("C$D@")),
          Evaluation.success("m2(new D)", "m2(d)")
        )
      )
    }

    "evaluate expression in package" - {
      val source =
        """package debug {
          |object EvaluateTest {
          |    def main(args: Array[String]): Unit = {
          |      println("Hello, World!")
          |    }
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "debug.EvaluateTest")(
        Breakpoint(4)(Evaluation.success("1 + 2", 3))
      )
    }

    "evaluate expression with Java util code" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertInMainClass(
        source,
        "EvaluateTest",
        3,
        "new java.util.ArrayList[String]().toString"
      )(_.exists(_ == "\"[]\""))
    }

    "return error message when expression is invalid" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 3, "1 ++ 2") { result =>
        result.left.exists { msg =>
          msg.format.contains("value ++ is not a member of Int")
        }
      }
    }

    "evaluate expression inside of a lambda" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    List(1).foreach(n => {
           |      println(n)
           |    })
           |    List(1).foreach { n =>
           |      println(n)
           |    }
           |  }
           |
           |  def m1(): Int = 9
           |}
           |""".stripMargin
      val evaluations =
        Seq(
          Breakpoint(6)(
            Evaluation.success("n", 1),
            Evaluation.success("m1()", 9)
          )
        ) ++
          (if (isScala3) Some(Breakpoint(9)()) else None) :+
          Breakpoint(9)(
            Evaluation.success("n", 1),
            Evaluation.success("m1()", 9)
          )

      assertInMainClass(source, "example.Main")(evaluations: _*)
    }

    "evaluate expression with breakpoint on an assignment" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val foo = new Foo
           |    println(foo.toString)
           |  }
           |}
           |
           |class Foo {
           |  val a = 1
           |  val b = 2
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(5)(Evaluation.success("1 + 2", 3)),
        Breakpoint(12)(Evaluation.success("a + 2", 3))
      )
    }

    "evaluate expression with breakpoint on method definition" - {
      val source =
        """class Foo {
          |  def bar(): String = "foobar"
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    new Foo().bar()
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(2)(Evaluation.success("1 + 2", 3))
      )
    }

    "evaluate expression definition" - {
      val source =
        """|object EvaluateTest {
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(3)(Evaluation.success("val x = 123", ()))
      )
    }

    "evaluate multi-line expression" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val a = 1
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(4)(
          Evaluation.success(
            """val b = 2
              |val c = 3
              |a + b + c
              |""".stripMargin,
            6
          )
        )
      )
    }

    "evaluate in default arguments" - {
      val source =
        """|object EvaluateTest {
           |  def main(args: Array[String]): Unit = {
           |    foo(3)()
           |  }
           |  def foo(x: Int)(
           |    y: Int = x + 1
           |  ): Unit = {
           |    println("foo")
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(6)(Evaluation.success("x + 1", 4))
      )
    }

    "evaluate inside local method" - {
      val source =
        """|package example
           |
           |object A {
           |  def main(args: Array[String]): Unit = {
           |    val x1 = 1
           |    def m1(name: String): String = {
           |      s"m$x1($name)"
           |    }
           |    def m2(): String = {
           |      s"m2()"
           |    }
           |    println(m1("foo"))
           |  }
           |}""".stripMargin
      assertInMainClass(source, "example.A")(
        Breakpoint(7)(
          Evaluation.success("m1(\"bar\")", "m1(bar)"),
          Evaluation.success("m2()", "m2()")
        )
      )
    }

    "evaluate inside multi-level nested local class and def" - {
      val source =
        """|package example
           |
           |class A {
           |  def m(): String = {
           |    val x1 = "x1"
           |    class B {
           |      def m(): String = {
           |        val x2 = "x2"
           |        def m(): String = {
           |          val x3 = "x3"
           |          class C {
           |            def m(): String = {
           |              val x4 = "x4"
           |              def m(): String = {
           |                x1 + x2 + x3 + x4
           |              }
           |              m()
           |              x1 + x2 + x3
           |            }
           |          }
           |          val c = new C
           |          c.m()
           |          x1 + x2
           |        }
           |        m()
           |        x1
           |      }
           |    }
           |    val b = new B
           |    b.m()
           |    ""
           |  }
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val a = new A
           |    a.m()
           |  }
           |}""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(30)( // in A#m
          Evaluation.success("new B")(_.startsWith("A$B$1@")), // captures x1
          Evaluation.success("(new B).m()", "x1")
        ),
        Breakpoint(25)( // in B#m
          Evaluation.success("x1", "x1"), // captured by B
          Evaluation.success("m()", "x1x2"), // captures x2
          Evaluation.success("this.m()", "x1"),
          Evaluation.success("A.this.m()", new NoSuchFieldException("$outer")),
          Evaluation.successOrIgnore("new B", isScala2)(
            _.startsWith("A$B$1@")
          ) // captures x1
        ),
        Breakpoint(22)( // in B#m#m
          Evaluation.success("x1", "x1"), // captured by B
          Evaluation.success("x2", "x2"), // captured by m
          Evaluation.success("m()", "x1x2"), // captures x2
          Evaluation.success("this.m()", "x1"), // captures x2
          Evaluation.successOrIgnore("new B", isScala2)(
            _.startsWith("A$B$1@")
          ), // captures x1
          Evaluation.success("new C")(
            _.startsWith("A$B$1$C$1@")
          ), // captures x2 and x3
          Evaluation.success("(new C).m()", "x1x2x3") // captures x2 and x3
        ),
        Breakpoint(17)( // in C#m
          Evaluation.successOrIgnore(
            "x1",
            "x1",
            isScala2
          ), // captured by B => $this.$outer.x1$1
          Evaluation.successOrIgnore(
            "x2",
            "x2",
            isScala2
          ), // captured by C => $this.x2$1
          Evaluation.successOrIgnore(
            "x3",
            "x3",
            isScala2
          ), // captured by C => $this.x3$1
          Evaluation.success("m()", "x1x2x3x4"), // captures x4
          Evaluation.success("this.m()", "x1x2x3"),
          Evaluation.successOrIgnore("B.this.m()", "x1", isScala2),
          Evaluation.successOrIgnore("new C", isScala2)(
            _.startsWith("A$B$1$C$1@")
          ), // captures x2 and x3
          Evaluation.successOrIgnore("new B", isScala2)(
            _.startsWith("A$B$1@")
          ), // captures x1
          Evaluation.success("new A")(_.startsWith("A@"))
        ),
        Breakpoint(15)( // in C#m#m
          Evaluation.successOrIgnore(
            "x1",
            "x1",
            isScala2
          ), // captured by B => $this.$outer.x1$1
          Evaluation.successOrIgnore(
            "x2",
            "x2",
            isScala2
          ), // captured by C => $this.x2$1
          Evaluation.successOrIgnore(
            "x3",
            "x3",
            isScala2
          ), // captured by C => $this.x3$1
          Evaluation.success("x4", "x4"), // captured by D => local x4$1
          Evaluation.success("m()", "x1x2x3x4"), // captures x4
          Evaluation.success("this.m()", "x1x2x3"),
          Evaluation.successOrIgnore("B.this.m()", "x1", isScala2),
          Evaluation.successOrIgnore("new C", isScala2)(
            _.startsWith("A$B$1$C$1@")
          ), // captures x2 and x3
          Evaluation.successOrIgnore("new B", isScala2)(
            _.startsWith("A$B$1@")
          ), // captures x1
          Evaluation.success("new A")(_.startsWith("A@"))
        )
      )
    }

    "evaluate captured local variable shadowing captured variable" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val x = "x1"
           |    def m(): String = {
           |      println(x) // captures x = "x1"
           |      val y = {
           |        val x = "x2"
           |        val z = {
           |          val x = "x3"
           |          def m(): String = {
           |            x // captures x = "x3"
           |          }
           |          m()
           |        }
           |        z
           |      }
           |      y
           |    }
           |    println(m())
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(15)(
          Evaluation.success("x", "x3"),
          Evaluation.success("m()", "x3")
        )
      )
    }

    "read and write mutable variables" - {
      val source =
        """|package example
           |
           |class A {
           |  private var x = 1
           |  def xx(): Int = {
           |    var y = 1
           |    var z = 1
           |    var u = 1
           |    x += 1
           |    def yy(): Int = {
           |      y += 1
           |      y
           |    }
           |    class B {
           |      def zz(): Int = {
           |        z += 1
           |        z
           |      }
           |    }
           |    val b = new B
           |    x * 100 + yy() + b.zz() + u
           |  }
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val a = new A
           |    println(a.xx())
           |  }
           |}
           |""".stripMargin

      assertInMainClass(source, "example.Main")(
        Breakpoint(9)(
          Evaluation.success("x", 1),
          Evaluation.success("u", 1),
          Evaluation.success("y", 1),
          // we can reassign x because it is a field
          Evaluation.successOrIgnore("x = 2", (), isScala2),
          Evaluation.successOrIgnore("x", 2, isScala2),
          Evaluation.successOrIgnore("x = x - 1", (), isScala2),
          Evaluation.successOrIgnore("x", 1, isScala2),
          Evaluation.successOrIgnore("x *= 2", (), isScala2),
          Evaluation.successOrIgnore("x", 2, isScala2),
          // we can reassign neither y nor u because they are fields
          Evaluation.failed("u = 2")(_ => true),
          if (isScala3) Evaluation.failed("y += 1")(_ => true)
          else Evaluation.success("y += 1", ()),
          Evaluation.success("new B")(_.startsWith("A$B$1@")),
          if (isScala3) Evaluation.success("yy()", 2)
          else Evaluation.success("yy()", 3)
        ),
        Breakpoint(11)(
          // captured by method m
          if (isScala3) Evaluation.success("y", 2)
          else Evaluation.success("y", 3),
          if (isScala3) Evaluation.failed("y += 1")(_ => true)
          else Evaluation.success("y += 1", ())
        ),
        Breakpoint(12)(
          if (isScala3) Evaluation.success("y", 3)
          else Evaluation.success("y", 5)
        ),
        Breakpoint(16)(
          // captured by class B
          Evaluation.successOrIgnore("z", 1, isScala2),
          Evaluation.failedOrIgnore("z += 1", isScala2)(_ => true)
        )
      )
    }

    "evaluate lazy variables" - {
      val source =
        """|package example
           |
           |object A {
           |  private lazy val x = 1
           |  def m(): Int = {
           |    lazy val y = 2
           |    x + y
           |  }
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    println(A.m())
           |  }
           |}
           |""".stripMargin

      assertInMainClass(source, "example.Main")(
        Breakpoint(7)(
          Evaluation.successOrIgnore("x", 1, isScala2),
          if (isScala3) Evaluation.failed("y")(_ => true)
          else Evaluation.success("y", 2),
          Evaluation.successOrIgnore(
            """|lazy val z = 2
               |z""".stripMargin,
            2,
            isScala2
          )
        )
      )
    }

    "evaluate private members in parent class" - {
      val source =
        """|package example
           |
           |abstract class BaseA {
           |  private val x: String = "x"
           |  private var y: String = "y"
           |  private lazy val z: String = "z"
           |  def m1: String = {
           |    val b = new B
           |    b.m3 + m2
           |  }
           |  private def m2: String = {
           |    y + z
           |  }
           |  private abstract class BaseB {
           |    def m3: String = {
           |      x
           |    }
           |  }
           |  private class B extends BaseB
           |}
           |
           |class A extends BaseA
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val a = new A
           |    println(a.m1)
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(9)(
          Evaluation.successOrIgnore("x", "x", isScala2),
          Evaluation.successOrIgnore("this.x", "x", isScala2),
          Evaluation.successOrIgnore("y", "y", isScala2),
          Evaluation.successOrIgnore("this.y = \"yy\"", (), isScala2),
          Evaluation.successOrIgnore("y", "yy", isScala2),
          Evaluation.successOrIgnore("z", "z", isScala2),
          Evaluation.successOrIgnore("m2", "yyz", isScala2)
        ),
        Breakpoint(16)(
          Evaluation.success("x", "x")
        )
      )
    }

    "evaluate type parameter list and multi parameter lists" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |
           |  def m1[X]: String = {
           |    "m1[X]"
           |  }
           |
           |  private def m2[X]: String = {
           |    "m2[X]"
           |  }
           |
           |  private def m3(x: Int)(y: String): String = {
           |    s"m3($x)($y)"
           |  }
           |
           |  private def m4[X, Y](x: X)(y: Y): String = {
           |    s"m4($x)($y)"
           |  }
           |
           |  private class A[X]
           |  private class B(x: Int)(y: String)
           |  private class C[X, Y](x: X)(y: Y)
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(5)(
          Evaluation.success("m1[String]", "m1[X]"),
          Evaluation.success("m1[A[Int]]", "m1[X]"),
          Evaluation.success("m2[String]", "m2[X]"),
          Evaluation.success("m3(1)(\"x\")", "m3(1)(x)"),
          Evaluation
            .successOrIgnore("m4[Int, String](1)(\"x\")", "m4(1)(x)", isScala2),
          Evaluation.success("new A[String]")(_.startsWith("Main$A@")),
          Evaluation.success("new B(2)(\"x\")")(_.startsWith("Main$B@")),
          Evaluation.success("new C[Int, String](2)(\"x\")")(
            _.startsWith("Main$C@")
          )
        )
      )
    }

    "evaluate tail-rec function" - {
      val source =
        """|object EvaluateTest {
           |  @scala.annotation.tailrec
           |  def f(x: Int): Int = {
           |    if (x <= 42) {
           |      x
           |    } else f(x/2)
           |  }
           |  def main(args: Array[String]): Unit = {
           |    val result = f(2)
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(5)(Evaluation.success("f(x)", 2))
      )
    }

    "keep working after success or failure" - {
      val source =
        """|object EvaluateTest {
           |  def main(args: Array[String]): Unit = {
           |    val result = 2
           |    println(result)
           |    println(result)
           |    println(result)
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(4)(Evaluation.success("result + 1", 3)),
        Breakpoint(5)(Evaluation.failed("resulterror")(_ => true)),
        Breakpoint(6)(Evaluation.success("result + 2", 4))
      )
    }

    "evaluate App block method" - {
      val source =
        """|object EvaluateTest extends App {
           |  val x = 1
           |  val y = {
           |    val msg = "Hello World!"
           |    println(msg)
           |    true
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 6, "msg.toString()")(
        _.exists(_ == "\"Hello World!\"")
      )
    }

    "evaluate at lambda start" - {
      val source =
        """|object EvaluateTest{
           |  def main(args: Array[String]): Unit = {
           |    val list = List(1, 2, 3)
           |    list.foreach { x =>
           |      println(x)
           |    }
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(4)(Evaluation.success("1 + 1", 2))
      )
    }

    "return exception as the result of evaluation" - {
      val source =
        """|object EvaluateTest{
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |
           |  def throwException(): Unit = {
           |    throw new Exception("error")
           |  }
           |}
           |""".stripMargin
      assertInMainClass(
        source,
        "EvaluateTest",
        3,
        "throwException()"
      )(
        _.exists(_.contains("\"java.lang.Exception: error\""))
      )
    }

    "evaluate in munit test" - {
      val source =
        """|class MySuite extends munit.FunSuite {
           |  def sum(list: List[Int]): Int = list.sum
           |
           |  test("sum of a few numbers") {
           |    assertEquals(sum(List(1,2,0)), 3)
           |  }
           |}""".stripMargin

      if (isScala31) {
        assertInTestSuite(source, "MySuite")(
          Breakpoint(5)(Evaluation.success("1 + 1", 2))
        )
      } else {
        assertInTestSuite(source, "MySuite")(
          Breakpoint(5)(), // the program stops twice...
          Breakpoint(5)(Evaluation.success("1 + 1", 2))
        )
      }
    }

    "evaluate lambdas" - {
      val source =
        """class Foo {
          |  val a = 1
          |  private val b = 2
          |  def bar() = {
          |    val c = 3
          |    println(s"a + b + c = ${a + b + c}")
          |  }
          |}
          |
          |object EvaluateTest {
          |  val a = 1
          |  private val b = 2
          |  def main(args: Array[String]): Unit = {
          |    val c = 3
          |    println(a + b + c)
          |    new Foo().bar()
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(6)(
          Evaluation.success("List(1, 2, 3).map(_ * 2).sum", 12),
          Evaluation.success("List(1, 2, 3).map(_ * a * b * c).sum", 36)
        ),
        Breakpoint(15)(
          Evaluation.success("List(1, 2, 3).map(_ * a * b * c).sum", 36)
        )
      )
    }

    "evaluate call to anonymous function" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val f = (s: String) => s.size
           |    println(f("Hello world"))
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(6)(
          Evaluation.success("f(\"foo\")")(_.contains("3"))
        )
      )
    }

    "evaluate call to method of generic class" - {
      val source =
        """|package example
           |
           |class Writer[T](f: T => Unit) {
           |  def write(value: T): Unit = {
           |    f(value)
           |  }
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val writer = new Writer[String](println(_))
           |    writer.write("Hello, World!")
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(5)(
          Evaluation.success("write(value)", ()),
          // Should it work without casting?
          // In contravariant case, we could find what's the expected type
          // In the covariant case, it is not possible to know what the precise return type is at runtime
          Evaluation.success("write(\"Hello\".asInstanceOf[T])", ())
        )
      )
    }

    "evaluate call to anonymous polymorphic function" - {
      val source =
        """|package example
           |
           |object Foo {
           |  def foo[A](f: String => A): A = {
           |    f("foo")
           |  }
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    Foo.foo(_.reverse)
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(5)(
          Evaluation.success("f(\"foo\")", "oof")
        )
      )
    }
  }
}
