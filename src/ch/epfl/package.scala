package ch.epfl

import scala.annotation.StaticAnnotation
import language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object inline {

  /** Annotation class for @sinline macro annotation. */
  final class sinline extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro InlineMacroAnnotation.impl
  }

  def sinline[T](body: T): T = macro InlineMacros.sinline[T]

  // Well reify does not work :/
  def treeString[T](body: T): String = macro InlineMacros.treeString[T]
}
