package ch.epfl

import scala.annotation.StaticAnnotation
import language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object scalact {

  /**
   * Partially evaluates the body (must be static) and returns an inline version of the
   * type T. All operations on the return type will be inlined and all non-generic
   * arguments @ct.
   */
  def ct[T](body: => T): T = ???

  /**
   * Prints the code of the partially evaluated body.
   *  This method is primarily used for debugging purposes.
   */
  def showCode(body: => Any): String = ???

  /**
   * Prints the code of the partially evaluated body.
   *  This method is primarily used for debugging purposes.
   */
  def debug(body: => Any): String = ???
}
