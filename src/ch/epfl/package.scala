package ch.epfl

import scala.annotation.StaticAnnotation
import language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object scalainline {

  /**
   * Partially evaluates the body (must be static) and returns an inline version of the
   * type T. All operations on the return type will be inlined and all non-generic
   * arguments @inline.
   */
  def ct[T](body: => T): T @ch.epfl.scalainline.inline = ???

}
