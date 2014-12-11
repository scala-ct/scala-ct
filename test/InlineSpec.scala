package ch.epfl

import org.scalatest.{ FlatSpec, ShouldMatchers }
import ch.epfl.scalainline._

import library._

class InlineSpec extends FlatSpec with ShouldMatchers {

  def i1: Int = 1
  def i2: Int = 2
  def d1: Double = 1.0
  def d2: Double = 2.0
  def d3: Double = 3.0
  def d4: Double = 4.0

  // TODO drop the inlinity warnings
  "Inline" should "partially evaluate @inline functions" in {
    @inline def simpleFunction(m: Int): String = if (m > 0) "Positive" else "Negative"

    simpleFunction(i1) should be("Positive")
    (showCode { simpleFunction(i1) }).replaceAll("\\$macro\\$\\d+", "") should be("""if (InlineSpec.this.i1.>(0))
        |  "Positive"
        |else
        |  "Negative" """.stripMargin.trim)
  }

  it should "evaluate if statements when there is an inline parameter that is constant" in {
    @inline def simpleFunction0(m: Int @inline): String = if (m > 0) "Positive" else "Negative"
    @inline def simpleFunction(m: Int @inline): String = simpleFunction0(m)

    simpleFunction(2) should be("Positive")
    (showCode { simpleFunction(2) }).replaceAll("\\$macro\\$\\d+", "") should be("\"Positive\"")
  }

  it should "promote parameters to inline when there is an inlinestatic annotation" in {
    @inline def simpleFunction0(m: Int @inlinestatic): String = if (m > 0) "Positive" else "Negative"
    @inline def simpleFunction(m: Int @inlinestatic): String = simpleFunction0(m)

    simpleFunction(2) should be("Positive")
    (showCode { simpleFunction(2) }).replaceAll("\\$macro\\$\\d+", "") should be("\"Positive\"")
  }

  it should "work with recursion" in {
    @inlinestatic def pow(base: Int, exp: Int @inlinestatic): Int = if (exp == 0) 1 else base * pow(base, exp - 1)
    def base: Int = 3
    def exp: Int = 2
    pow(3, 2) should be(9)
    pow(base, exp) should be(9)

    (showCode { pow(3, 2) }).toString.replaceAll("\\$macro\\$\\d+", "") should be("(3).*((3).*(1))")
    (showCode { pow(base, exp) }).toString.replaceAll("\\$macro\\$\\d+", "") should be("""pow(base, exp)""")
    (showCode { pow(base, 2) }).toString.replaceAll("\\$macro\\$\\d+", "") should be("""base.*(base.*(1))""")
  }

  it should "`inline` should promote to @inline" in {

    val x = inline(1)
    (x + 1) should be(2)

    showCode((x + 1)) should be("2")
    (showCode {
      val x = inline(1)
      x + 1
    }).toString.replaceAll("\\$macro\\$\\d+", "") should be("""{
      |  val x: Int @ch.epfl.scalainline.inline = 1;
      |  2
      |}""".stripMargin)
  }

  it should "evaluate inline lists" in {
    val list = inline(List)(inline(1))
    list.head should be(1)
    showCode { inline(List)(inline(1)).head } should be("""1""")
  }

  it should "evaluate inline lists with trees" in {
    inline(List)(1, 2).head should be(1)
    inline(List)(inline(1), 2).head should be(1)
    showCode(inline(List)(1, 2).head) should be("""1""")
    showCode(inline(List)(inline(1), 2).head) should be("""1""")
  }

  it should "be able to map over trees" in {
    val l1 = inline(List)(1, 2)
    val l2 = inline(List)(i1, 2)
    l1.map((x: Int) => x + 2).head should be(3)
    l2.map((x: Int) => x + 2).head should be(3)
    showCode(l1.map((x: Int) => x + 2).head).replaceAll("\\$macro\\$\\d+", "") should be("""(1).+(2)""".stripMargin)
    showCode(l2.map((x: Int) => x + 2).head).replaceAll("\\$macro\\$\\d+", "") should be("""InlineSpec.this.i1.+(2)""".stripMargin)

  }

  it should "respect @inline annotations on classes" in {
    import Numeric._

    val num = DoubleNumeric
    num.plus(num.one, num.one) should be(2.0)

    val l1 = inline(List)(d1, d2)
    showCode(num.plus(d1, num.one)).replaceAll("\\$macro\\$\\d+", "") should be("InlineSpec.this.d1.+(1.0)")
  }

  it should "be able to zip two lists" in {
    (inline(List)(1, 2).zip(inline(List)(3, 4))) should be(new Cons(new Tuple2(1, 3), new Cons(new Tuple2(2, 4), Nil)))
    // TODO types should be right
    showCode { inline(List)(1, 2).zip(inline(List)(3, 4)) } should be("new Cons[Tuple2[T, U]](new Tuple2(1, 3), new Cons[Tuple2[T, U]](new Tuple2(2, 4), Nil))")
  }

  it should "be able to do numeric operations on lists" in {
    import Numeric._
    @inline def addOne[V](v1: List[V] @inline, num: Numeric[V]): List[V] = {
      v1.map((x: V) => num.plus(x, num.one))
    }
    val l1 = inline(List)(1.1, 2.2)
    val l2 = inline(List)(d1, d2)

    addOne(l1, DoubleNumeric).head should be(2.1)
    showCode(addOne(l1, DoubleNumeric).head).replaceAll("\\$macro\\$\\d+", "") should be("""1.1.+(1.0)""")

    addOne(l2, DoubleNumeric).head should be(2)
    showCode(addOne(l2, DoubleNumeric).head).replaceAll("\\$macro\\$\\d+", "") should be("""InlineSpec.this.d1.+(1.0)""")
  }

  it should "be able to do fold on lists" in {
    @inline def foldList[V](v1: List[V] @inline, num: Numeric[V]): V = {
      v1.foldLeft[V](num.zero, (agg: V, v: V) => num.plus(agg, v))
    }
    val l1 = inline(List)(1.1, 2.2)
    val l2 = inline(List)(d1, d2)

    foldList(l1, DoubleNumeric) should be(3.3000000000000003)
    showCode(foldList(l1, DoubleNumeric)).replaceAll("\\$macro\\$\\d+", "") should be("0.0.+(1.1).+(2.2)")
    foldList(l2, DoubleNumeric) should be(3.0)
    showCode(foldList(l2, DoubleNumeric)).replaceAll("\\$macro\\$\\d+", "") should be("0.0.+(InlineSpec.this.d1).+(InlineSpec.this.d2)")
  }

  it should "be able to do a dot product" in {
    // TODO Multiple parameter lists
    // TODO Vector
    @inlinestatic
    def dot[V](v1: List[V] @inlinestatic, v2: List[V] @inlinestatic, num: Numeric[V]): V = {
      (v1 zip v2).foldLeft(num.zero, { (sum: V, v: Tuple2[V, V]) =>
        num.plus(sum, num.times(v._1, v._2))
      })
    }

    val l1 = inline(List)(1.1, 2.2)
    val l2 = inline(List)(3.1, 4.2)
    val l3 = inline(List)(d1, d2)
    val l4 = inline(List)(d3, d4)

    dot(l1, l2, DoubleNumeric) should be(12.650000000000002)
    showCode((dot(l1, l2, DoubleNumeric))) should be("0.0.+(1.1.*(3.1)).+(2.2.*(4.2))")
    dot(l3, l4, DoubleNumeric) should be(11)
    showCode((dot(l3, l4, DoubleNumeric))) should be("0.0.+(InlineSpec.this.d1.*(InlineSpec.this.d3)).+(InlineSpec.this.d2.*(InlineSpec.this.d4))")

    val l5 = List(d1, d2)
    val l6 = List(d3, d4)
    showCode(dot(l5, l6, DoubleNumeric)) should be("dot[Double](l5, l6, library.DoubleNumeric)")
  }

}
