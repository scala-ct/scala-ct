package library

sealed trait List[+T] {
  def head: T
  def tail: List[T]

  final def zip[U](l: List[U]): List[Tuple2[T, U]] = if (this == Nil) {
    Nil
  } else {
    new Cons[Tuple2[T, U]](
      new Tuple2(
        head,
        l.head),
      tail.zip(l.tail))
  }

  def map[U](f: T => U): List[U] = if (this == Nil) {
    Nil
  } else {
    new Cons(f(head), tail.map(f))
  }

  def foldLeft[U](z: U, f: (U, T) => U): U = {
    if (this == Nil)
      z
    else
      tail.foldLeft(f(z, head), f)
  }
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def equals(that: Any): Boolean = {
    val thatList = that.asInstanceOf[List[Any]]
    thatList.head == head && thatList.tail == tail
  }
}
object Nil extends List[Nothing] {
  def head: Nothing = throw new RuntimeException("Head of the empty list!")
  def tail: Nothing = throw new RuntimeException("Tail of the empty list!")
}

/*
 * Does not work with the interpreter!
 */
object List {
  def apply[T](x1: T, x2: T, x3: T): List[T] =
    new Cons[T](x1, new Cons[T](x2, new Cons(x3, Nil)))
  def apply[T](x1: T, x2: T): List[T] =
    new Cons[T](x1, new Cons[T](x2, Nil))
  def apply[T](x1: T): List[T] =
    new Cons(x1, Nil)
  def apply[T](): List[T] =
    Nil

  def zip[T, U](l1: List[T], l2: List[U]): List[Tuple2[T, U]] = if (l1 == Nil) {
    Nil
  } else {
    new Cons[Tuple2[T, U]](
      new Tuple2(
        l1.head,
        l2.head),
      zip(l1.tail, l2.tail))
  }

}

class Tuple2[+T, +U](val _1: T, val _2: U) {
  override def equals(x: Any): Boolean = {
    if (x.isInstanceOf[Tuple2[Any, Any]]) {
      val that = x.asInstanceOf[Tuple2[Any, Any]]
      (this._1 equals that._1) && (this._2 == that._2)
    } else false
  }
  override def toString: String = s"(${_1},${_2})"
}

import ch.epfl.scalainline._
object Numeric {
  @inline
  implicit def dnum: Numeric[Double] @inline = DoubleNumeric
}
trait Numeric[T] {
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def times(x: T, y: T): T
  def fromInt(x: Int): T

  def zero(): T
  def one(): T

  class Ops(lhs: T) {
    @inline def +(rhs: T @inlinestatic) = plus(lhs, rhs)
    @inline def -(rhs: T @inlinestatic) = minus(lhs, rhs)
    @inline def *(rhs: T @inlinestatic) = times(lhs, rhs)
  }

  @inline implicit def mkNumericOps(lhs: T): Ops = new Ops(lhs)
}

object DoubleNumeric extends Numeric[Double] {
  @inline def plus(x: Double, y: Double): Double = x + y
  @inline def minus(x: Double, y: Double): Double = x - y
  @inline def times(x: Double, y: Double): Double = x * y
  @inline def fromInt(x: Int): Double = x
  @inline def one: Double = 1.0
  @inline def zero: Double = 0.0
}
