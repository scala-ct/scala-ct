package ch.epfl.scalact.examples
import reflect.macros.blackbox.Context
import ch.epfl.scalact.ct
// Translation for vararg methods
object Math {
  def min(xs: Int*): Int = macro min_impl
  def min_impl(c: Context)(xs: c.Expr[Int]*): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](xs.map(_.tree) match {
      case Typed(_, Ident(typeNames.WILDCARD_STAR)) :: Nil =>
        q"_root_.ch.epfl.scalact.examples.Math.min_D(${xs.head})"
      case _ => q"_root_.ch.epfl.scalact.examples.Math.min_CT(List(..${xs.toList}))"
    })
  }

  def min_D(vs: Int*) = vs.tail.foldLeft(vs.head)({
    (cmin, v) => if (v > cmin) v else cmin
  })

  @ct def min_CT(vs: Seq[Int] @ct) = vs.tail.foldLeft(vs.head)({
    (cmin, v) => if (v < cmin) v else cmin
  })
}
