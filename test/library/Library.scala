package library

sealed trait List[+T] {
  def head: T
  def tail: List[T]

  def zip[U](l: List[U]): List[Tuple2[T, U]] = if (this == Nil) {
    Nil
  } else {
    new Cons[Tuple2[T, U]](
      new Tuple2(
        head,
        l.head),
      tail.zip(l.tail))
  }

  def zipWithIndex: List[Tuple2[T, Int]] = zipWithStartingIndex(0)

  def zipWithStartingIndex(starting: Int): List[Tuple2[T, Int]] = if (this == Nil) {
    Nil
  } else {
    new Cons(
      new Tuple2(
        head,
        starting),
      tail.zipWithStartingIndex(starting + 1))
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

  def length: Int = if (this == Nil) 0 else tail.length + 1
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def equals(that: Any): Boolean = {
    val thatList = that.asInstanceOf[List[Any]]
    thatList.head == head && thatList.tail == tail
  }
}

object Nil extends List[Nothing] {
  def head: Nothing = throw new RuntimeException("Head of the empty list!")
  def tail: List[Nothing] = throw new RuntimeException("Tail of the empty list!")
}

/*
 * Does not work with the interpreter!
 */
object List {
  def apply[T](x1: T, x2: T, x3: T, x4: T): List[T] =
    new Cons(x1, new Cons(x2, new Cons(x3, new Cons(x4, Nil))))
  def apply[T](x1: T, x2: T, x3: T): List[T] =
    new Cons(x1, new Cons(x2, new Cons(x3, Nil)))
  def apply[T](x1: T, x2: T): List[T] =
    new Cons(x1, new Cons(x2, Nil))
  def apply[T](x1: T): List[T] =
    new Cons(x1, Nil)
  def apply[T](): List[T] =
    Nil
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

import ch.epfl.scalact._

object Numeric {
  @ct implicit def dnum(): Numeric[Double] @ct = DoubleNumeric
  @ct implicit def dnumct(): Numeric[Double @ct] @ct = DoubleNumericCT
}

trait Numeric[T] {
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def times(x: T, y: T): T
  def fromInt(x: Int): T

  def zero(): T
  def one(): T

  class Ops(lhs: T) {
    @ct def +(rhs: T) = plus(lhs, rhs)
    @ct def -(rhs: T) = minus(lhs, rhs)
    @ct def *(rhs: T) = times(lhs, rhs)
  }

  @ct implicit def mkNumericOps(lhs: T): Ops = new (Ops @ ct)(lhs)
}

object DoubleNumeric extends Numeric[Double] @ct {
  @ct def plus(x: Double, y: Double): Double = x + y
  @ct def minus(x: Double, y: Double): Double = x - y
  @ct def times(x: Double, y: Double): Double = x * y
  @ct def fromInt(x: Int): Double = x
  @ct def one: Double = 1.0
  @ct def zero: Double = 0.0
}

object DoubleNumericCT extends Numeric[Double @ct] @ct {
  @ct def plus(x: Double @ct, y: Double @ct): Double @ct = x + y
  @ct def minus(x: Double @ct, y: Double @ct): Double @ct = x - y
  @ct def times(x: Double @ct, y: Double @ct): Double @ct = x * y
  @ct def fromInt(x: Int @ct): Double = x
  @ct def one: Double @ct = ct(1.0)
  @ct def zero: Double @ct = ct(0.0)
}
