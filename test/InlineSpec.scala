package ch.epfl

import org.scalatest.{ FlatSpec, ShouldMatchers }
import ch.epfl.scalact._

import library._

class InlineSpec extends FlatSpec with ShouldMatchers {

  def i1: Int = 1
  def i2: Int = 2
  def d1: Double = 1.0
  def d2: Double = 2.0
  def d3: Double = 3.0
  def d4: Double = 4.0

  "Inline" should "partially evaluate @ct functions" in {
    @ct def simpleFunction(m: Int): String = if (m > 0) "Positive" else "Negative"

    simpleFunction(i1) should be("Positive")
    (showCode { simpleFunction(i1) }).replaceAll("\\$macro\\$\\d+", "") should be("""if (InlineSpec.this.i1.>(0))
        |  "Positive"
        |else
        |  "Negative" """.stripMargin.trim)
  }

  it should "evaluate if statements when there is an inline parameter that is constant" in {
    @ct def simpleFunction0(m: Int @ct): String = if (m > 0) "Positive" else "Negative"
    @ct def simpleFunction(m: Int @ct): String = simpleFunction0(m)

    simpleFunction(2) should be("Positive")
    (showCode { simpleFunction(2) }).replaceAll("\\$macro\\$\\d+", "") should be("\"Positive\"")
  }

  it should "work with recursion" in {
    @ct def pow(base: Int, exp: Int @ct): Int = if (exp == 0) 1 else base * pow(base, exp - 1)
    def base: Int = 3
    def exp: Int = 2
    pow(3, 2) should be(9)

    (showCode { pow(3, 2) }).toString.replaceAll("\\$macro\\$\\d+", "") should be("(3).*((3).*(1))")
    (showCode { pow(base, 2) }).toString.replaceAll("\\$macro\\$\\d+", "") should be("""base.*(base.*(1))""")
  }

  it should "`inline` should promote to @ct" in {

    val x = ct(1)
    (x + 1) should be(2)

    showCode((x + 1)) should be("2")
    (showCode {
      val x = ct(1)
      x + 1
    }).toString.replaceAll("\\$macro\\$\\d+", "") should be("""{
      |  val x: Int @ch.epfl.scalact.ct = 1;
      |  2
      |}""".stripMargin)
  }

  it should "evaluate inline lists" in {
    val list = ct(List)(ct(1))
    list.head should be(1)
    showCode { ct(List)(ct(1)).head } should be("""1""")
  }

  it should "evaluate inline lists with trees" in {
    ct(List)(1, 2).head should be(1)
    ct(List)(ct(1), 2).head should be(1)
    showCode(ct(List)(1, 2).head) should be("""1""")
    showCode(ct(List)(ct(1), 2).head) should be("""1""")
  }

  it should "be able to map over trees" in {
    val l1 = ct(List)(1, 2)
    val l2 = ct(List)(i1, 2)
    l1.map((x: Int) => x + 2).head should be(3)
    l2.map((x: Int) => x + 2).head should be(3)
    showCode(l1.map((x: Int) => x + 2).head).replaceAll("\\$macro\\$\\d+", "") should be("""(1).+(2)""".stripMargin)
    showCode(l2.map((x: Int) => x + 2).head).replaceAll("\\$macro\\$\\d+", "") should be("""InlineSpec.this.i1.+(2)""".stripMargin)

  }

  it should "respect @ct annotations on classes" in {
    import Numeric._

    Numeric.dnum.plus(Numeric.dnum.one, Numeric.dnum.one) should be(2.0)

    val l1 = ct(List)(d1, d2)
    showCode(Numeric.dnum.plus(d1, Numeric.dnum.one)).replaceAll("\\$macro\\$\\d+", "") should be("InlineSpec.this.d1.+(1.0)")
  }

  it should "be able to determine the length of an inline list" in {
    ct(List)(1, 2).length should be(2)
    // TODO types should be right
    showCode { ct(List)(1, 2).length } should be("2")
  }

  it should "be able to zip two lists" in {
    (ct(List)(1, 2).zip(ct(List)(3, 4))) should be(new Cons(new Tuple2(1, 3), new Cons(new Tuple2(2, 4), Nil)))
    // TODO types should be right
    showCode { ct(List)(1, 2).zip(ct(List)(3, 4)) } should be("new Cons[Tuple2[T, U]](new Tuple2(1, 3), new Cons[Tuple2[T, U]](new Tuple2(2, 4), Nil))")
  }

  it should "be able to do numeric operations on lists" in {
    import Numeric._
    @ct def addOne[V](v1: List[V] @ct, num: Numeric[V] @ct): List[V] = {
      v1.map((x: V) => num.plus(x, num.one))
    }
    val l1 = ct(List)(1.1, 2.2)
    val l2 = ct(List)(d1, d2)

    addOne(l1, Numeric.dnum).head should be(2.1)
    showCode(addOne(l1, Numeric.dnum).head).replaceAll("\\$macro\\$\\d+", "") should be("""1.1.+(1.0)""")

    addOne(l2, Numeric.dnum).head should be(2)
    showCode(addOne(l2, Numeric.dnum).head).replaceAll("\\$macro\\$\\d+", "") should be("""InlineSpec.this.d1.+(1.0)""")
  }

  it should "be able to do fold on lists" in {
    @ct def foldList[V](v1: List[V] @ct, num: Numeric[V]): V = {
      v1.foldLeft[V](num.zero, (agg: V, v: V) => num.plus(agg, v))
    }
    val l1 = ct(List)(1.1, 2.2)
    val l2 = ct(List)(d1, d2)

    foldList(l1, Numeric.dnum) should be(3.3000000000000003)
    showCode(foldList(l1, Numeric.dnum)).replaceAll("\\$macro\\$\\d+", "") should be("0.0.+(1.1).+(2.2)")
    foldList(l2, Numeric.dnum) should be(3.0)
    showCode(foldList(l2, Numeric.dnum)).replaceAll("\\$macro\\$\\d+", "") should be("0.0.+(InlineSpec.this.d1).+(InlineSpec.this.d2)")
  }

  it should "work with the min function" in {
    @ct def min_CT(vs: List[Int] @ct) = vs.tail.foldLeft[Int](vs.head, {
      (cmin, v) => if (v < cmin) v else cmin
    })

    showCode(min_CT(ct(List)(1, 2, 3))) should be("""if ((3).<(if ((2).<(1))
    |  2
    |else
    |  1))
    |  3
    |else
    |  if ((2).<(1))
    |    2
    |  else
    |    1""".stripMargin)
  }

  it should "be able to do a dot product" in {
    @ct def dot[V](v1: List[V] @ct, v2: List[V] @ct, num: Numeric[V] @ct): V = {
      (v1 zip v2).foldLeft(num.zero, { (sum: V, v: Tuple2[V, V]) =>
        num.plus(sum, num.times(v._1, v._2))
      })
    }
    val l3 = ct(List)(d1, d2)
    val l4 = ct(List)(d3, d4)
    val l1 = ct(List)(1.1, 2.2)
    val l2 = ct(List)(3.1, 4.2)
    val l5 = ct(List)(ct(1.1), ct(2.2))
    val l6 = ct(List)(ct(3.1), ct(4.2))

    // TODO Fix the multiple parameter list for num to be added implicitly
    dot(l1, l2, Numeric.dnum) should be(12.650000000000002)
    showCode((dot(l1, l2, ct(Numeric).dnum))) should be("0.0.+(1.1.*(3.1)).+(2.2.*(4.2))")
    dot(l3, l4, Numeric.dnum) should be(11)
    showCode((dot(l3, l4, Numeric.dnum))) should be("0.0.+(InlineSpec.this.d1.*(InlineSpec.this.d3)).+(InlineSpec.this.d2.*(InlineSpec.this.d4))")

    // Different implicit. This would be covered if overloading resolution worked.
    dot(l5, l6, Numeric.dnumct) should be(12.650000000000002)
    showCode((dot(l5, l6, Numeric.dnumct))) should be("12.650000000000002")
  }

  it should "inline new access" in {
    showCode(new (Cons[Int]@ ct)(1, Nil).head) should be("1")
    showCode(new (Tuple2[Int, Int]@ ct)(1, 1)._1) should be("1")
  }

  @ct def cPlus(lhs: Tuple2[Double, Double] @ct, rhs: Tuple2[Double, Double] @ct): Tuple2[Double, Double] @ct =
    new (Tuple2[Double, Double]@ ct)(lhs._1 + rhs._1, lhs._2 + rhs._2)

  @ct def cMinus(lhs: Tuple2[Double, Double] @ct, rhs: Tuple2[Double, Double] @ct): Tuple2[Double, Double] @ct =
    new (Tuple2[Double, Double]@ ct)(lhs._1 - rhs._1, lhs._2 - rhs._2)

  @ct def cTimes(lhs: Tuple2[Double, Double] @ct, rhs: Tuple2[Double, Double] @ct): Tuple2[Double, Double] @ct =
    new (Tuple2[Double, Double]@ ct)(lhs._1 * rhs._1 - lhs._2 * rhs._2, lhs._1 * rhs._2 + lhs._2 * rhs._1)

  @ct def concat[T](xs: List[T] @ct, ys: List[T] @ct): List[T] @ct =
    if (xs == ct(Nil)) ys
    else new (Cons[T]@ ct)(xs.head, concat(xs.tail, ys))

  @ct def unzip[T, U](xs: List[Tuple2[T, U] @ct] @ct): Tuple2[List[T] @ct, List[U] @ct] @ct =
    if (xs == ct(Nil)) new (Tuple2[List[T]@ ct, List[U]@ ct]@ ct)(ct(Nil), ct(Nil))
    else {
      val rest = unzip(xs.tail)
      new (Tuple2[List[T]@ ct, List[U]@ ct]@ ct)(
        new (Cons[T]@ ct)(xs.head._1, rest._1), new (Cons[U]@ ct)(xs.head._2, rest._2))
    }

  @ct def even[T](xs: List[T] @ct): List[T] @ct =
    if (xs == Nil) Nil
    else new (Cons[T]@ ct)(xs.head, even(xs.tail.tail))

  @ct def odd[T](xs: List[T] @ct): List[T] @ct =
    if (xs == Nil) Nil
    else new (Cons[T]@ ct)(xs.tail.head, odd(xs.tail.tail))

  @ct def omega(k: Int @ct, N: Int @ct): Tuple2[Double, Double] @ct = {
    val kth = k * -2.0 * 3.141592653589793 / N
    new (Tuple2[Double, Double]@ ct)(
      ct(java.lang.Math.cos(kth)), ct(java.lang.Math.sin(kth)))
  }

  it should "allow all prerequisite functions for the butterfly networks" in {
    // Math
    showCode(omega(2, 1)._1) should be("1.0")
    showCode(omega(1, 2)._2) should be("-1.2246467991473532E-16")
    showCode(cTimes(omega(1, 2), omega(2, 1))) should be("new library.Tuple2[scala.Double, scala.Double] @ct(5.99903913064743E-32, -6.123233995736766E-16)")
    showCode(cMinus(omega(1, 2), omega(2, 1))) should be("new library.Tuple2[scala.Double, scala.Double] @ct(-1.0, -5.99903913064743E-32)")
    showCode(cPlus(omega(1, 2), omega(2, 1))) should be("new library.Tuple2[scala.Double, scala.Double] @ct(0.0, 3.6739403974420594E-16)")

    // Lists
    val l1 = ct(List)(1, 2)
    val l2 = ct(List)(3, 4)
    val tupled = ct(List)(new (Tuple2[Int, Int]@ ct)(1, 2))
    (l1 zip l2).zipWithIndex.head._2 should be(0)
    showCode((l1 zip l2).zipWithIndex.tail.head._2) should be("1")
    showCode((l1 zip l2).zipWithIndex.tail.head._2 + 1) should be("2")
    showCode((l1 zip l2).zipWithIndex.map(v => v._2 + 2)) should be("new Cons(2, new Cons(3, Nil))")
    showCode((tupled zip tupled).zipWithIndex.map(v => v._2 + 1)) should be("new Cons(1, Nil)")
    showCode((tupled zip tupled).zipWithIndex.map(x => ct(x._2))) should be("new Cons(0, Nil)")

    showCode(even(l1).head) should be("1")
    showCode(odd(l1).head) should be("2")
    showCode(concat(l1, l2)) should be("new library.Cons[T] @ct(1, new library.Cons[T] @ct(2, new Cons(3, new Cons(4, Nil))))")
    showCode(unzip(tupled)) should be("new library.Tuple2[library.List[T] @ct, library.List[U] @ct] @ct(new library.Cons[T] @ct(1, library.Nil), new library.Cons[U] @ct(2, library.Nil))")

  }

  it should "be able to handle butterfly networks" in {

    @ct def fft(xs: List[Tuple2[Double, Double] @ct] @ct): List[Tuple2[Double, Double] @ct] @ct =
      if (xs.length == 1) xs
      else {
        val N = xs.length
        val evenValues = fft(even(xs))
        val oddValues = fft(odd(xs))
        val tupled = unzip((evenValues zip oddValues).zipWithIndex.map({ v =>
          val x = v._1._1
          val y = v._1._2
          val k = v._2
          val z = cTimes(omega(k, N), y)
          new (Tuple2[Tuple2[Double, Double]@ ct, Tuple2[Double, Double]@ ct]@ ct)(cPlus(x, z), cMinus(x, z))
        }))
        concat(tupled._1, tupled._2)
      }
    showCode(fft(ct(List)(new (Tuple2[Double, Double]@ ct)(1.1, 2.2)))) should be(
      "new Cons(new library.Tuple2[scala.Double, scala.Double] @ct(1.1, 2.2), Nil)")
    showCode(fft(ct(List)(new (Tuple2[Double, Double]@ ct)(1.1, 2.2), new (Tuple2[Double, Double]@ ct)(1.1, 2.2)))) should be(
      "new library.Cons[T] @ct(new library.Tuple2[scala.Double, scala.Double] @ct(1.1.+(-0.0), 2.2.+(2.2)), new library.Cons[U] @ct(new library.Tuple2[scala.Double, scala.Double] @ct(1.1.-(-0.0), 2.2.-(2.2)), library.Nil))")

    showCode({ // LMS Benchmark
      val out = fft(ct(List)(
        new (Tuple2[Double, Double]@ ct)(1.0, 0.0),
        new (Tuple2[Double, Double]@ ct)(1.0, 0.0)))
      val el0 = out.head
      val el1 = out.tail.head
      Array(el0._1, el0._2, el1._1, el1._2)
    }) should be("""|{
      |  val out: library.Cons[T @ch.epfl.scalact.dynamic] @ch.epfl.scalact.ct = new library.Cons[T] @ct(new library.Tuple2[scala.Double, scala.Double] @ct(1.0.+(-0.0), 0.0.+(0.0)), new library.Cons[U] @ct(new library.Tuple2[scala.Double, scala.Double] @ct(1.0.-(-0.0), 0.0.-(0.0)), library.Nil));
      |  val el0: library.Tuple2 @ch.epfl.scalact.ct = new library.Tuple2[scala.Double, scala.Double] @ct(1.0.+(-0.0), 0.0.+(0.0));
      |  val el1: library.Tuple2 @ch.epfl.scalact.ct = new library.Tuple2[scala.Double, scala.Double] @ct(1.0.-(-0.0), 0.0.-(0.0));
      |  scala.Array.apply(1.0.+(-0.0), 0.0.+(0.0))
      |}""".stripMargin)

  }

}
