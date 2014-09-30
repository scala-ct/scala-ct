package ch.epfl

import org.scalatest.{ FlatSpec, ShouldMatchers }
import ch.epfl.inline._

class InlineSpec extends FlatSpec with ShouldMatchers {

  "Inline" should "work with only vals" in {

    @sinline val x = List(1, 2, 3)

    x should be(List(1, 2, 3))
  }

  it should "inline the if construct if the arguments are static" in {
    @sinline val x: Int = 1
    treeString(sinline(if (x > 3) "Yey!" else "Nope!")) should be("""Typed(Literal(Constant("Nope!")), TypeTree())""")
  }

}
