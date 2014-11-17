package ch.epfl

import org.scalatest.{ FlatSpec, ShouldMatchers }
import ch.epfl.scalainline._

class InlineSpec extends FlatSpec with ShouldMatchers {

  def reify[T](x: => T) = x

  "Inline" should "partially evaluate @inline functions" in {
    @inline def simpleFunction(m: Int): String = if (m > 0) "Positive" else "Negative"

    simpleFunction(1) should be("Positive")
    (reify { simpleFunction(1) }).replaceAll("\\$macro\\$\\d+", "") should be("""{
    |  val m: Int = 1;
    |  if (m.>(0))
    |    "Positive"
    |  else
    |    "Negative"
    |}""".stripMargin)
  }

  it should "evaluate if statements when there is an inline parameter that is constant" in {
    @inline def simpleFunction0(m: Int @inlineable): String = if (m > 0) "Positive" else "Negative"
    @inline def simpleFunction(m: Int @inline): String = simpleFunction0(m)

    simpleFunction(2) should be("Positive")
    (reify { simpleFunction(2) }).replaceAll("\\$macro\\$\\d+", "") should be("""{
    |  val m: Int @ch.epfl.scalainline.inline = 2;
    |  {
    |    val m: Int @ch.epfl.scalainline.inline = 2;
    |    "Positive"
    |  }
    |}""".stripMargin)
  }

  it should "work with recursion" in {
    @inline def pow(base: Int, exp: Int @inline): Int = if (exp == 0) 1 else base * pow(base, exp - 1)

    pow(3, 2) should be(9)
    (reify { pow(3, 2) }).toString.replaceAll("\\$macro\\$\\d+", "") should be("""{
    |  val base: Int = 3;
    |  val exp: Int @ch.epfl.scalainline.inline = 2;
    |  base.*({
    |    val base: Int = base;
    |    val exp: Int @ch.epfl.scalainline.inline = 1;
    |    base.*({
    |      val base: Int = base;
    |      val exp: Int @ch.epfl.scalainline.inline = 0;
    |      1
    |    })
    |  })
    |}""".stripMargin)
  }

}
