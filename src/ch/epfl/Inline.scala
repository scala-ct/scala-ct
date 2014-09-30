package ch.epfl.inline

import scala.annotation.StaticAnnotation
import language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.interpreter._

/** Annotation for carrying the body of the macro annotation. */
final class body[T](body: T) extends StaticAnnotation

object InlineMacros {
  // Just takes the body of the valdef from the annotation.
  def valImpl[T](c: Context): c.Expr[T] = {
    import c.universe._
    val bodyAnnotation =
      c.macroApplication.symbol.annotations.filter(_.tree.tpe <:< c.typeOf[body[_]]).head
    val body = bodyAnnotation.scalaArgs.head
    c.Expr[T](body)
  }

  def sinline[T](c: Context)(body: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    def static(tree: c.Tree): Boolean = {
      // there are no captured identifiers
      true
    }

    val tp = body.tree.tpe
    // check and interpret
    val res = body.tree match {
      case If(cond, t, e) if static(cond) =>
        if (interpret(c)(cond).asInstanceOf[Boolean]) t else e
      case If(cond, t, e) => c.abort(body.tree.pos,
        "sinline[T](body: T) can be used only with if statements where all identifiers in the condition are static.")
      case _ => c.abort(body.tree.pos,
        "@sinline can be used only with if statements.")
    }
    c.Expr[T](res)
  }

  def treeString[T](c: Context)(body: c.Expr[T]): c.Expr[String] = {
    import c.universe._
    c.Expr[String](q"${showRaw(body.tree)}")
  }
}

/** Companion object implementing @sinline macro annotation. */
private object InlineMacroAnnotation {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val inputs = annottees.map(_.tree).toList
    val outputs = inputs match {
      case (vd: ValDef) :: Nil if vd.mods.hasFlag(Flag.LAZY) =>
        c.abort(vd.pos, "@sinline can not be applied to lazy vals.")
      case (vd: ValDef) :: Nil if vd.mods.hasFlag(Flag.MUTABLE) =>
        c.abort(vd.pos, "@sinline can not be applied to vars.")
      case (vd: ValDef) :: Nil =>
        List(q"""
          @body[${vd.tpt}](${vd.rhs})
          def x: ${vd.tpt} = macro ch.epfl.inline.InlineMacros.valImpl[${vd.tpt}]
        """)
      case _ => inputs
    }

    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}
