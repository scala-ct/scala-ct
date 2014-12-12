package ch.epfl

import org.scalatest.{ FlatSpec, ShouldMatchers }
import ch.epfl.scalainline._
import Numeric.Implicits._
import math._
import scala.collection.immutable.Vector

/*class CaseStudiesSpec extends FlatSpec with ShouldMatchers {
  // FFT example
  def omega(k: Int, N: Int): Complex = {
    val kth = -2.0 * k * math.Pi / N
    Complex(cos(kth), sin(kth))
  }

  @inline
  case class Complex(re: Double, im: Double) {
    def +(that: Complex): Complex =
      Complex(this.re + that.re, this.im + that.im)
    def -(that: Complex): Complex =
      Complex(this.re - that.re, this.im - that.im)
    def *(that: Complex): Complex =
      Complex(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)
  }

  @inline
  def splitEvenOdd[T](xs: List[T] @inlinestatic): (List[T] @inlinestatic, List[T] @inlinestatic) @inlinestatic = (xs: @unchecked) match {
    case e :: o :: xt =>
      val (es, os) = splitEvenOdd(xt)
      ((e :: es), (o :: os))
    case Nil => (Nil, Nil)
  }

  @inline
  def mergeEvenOdd[T](even: List[T] @inlinestatic, odd: List[T] @inlinestatic): List[T] = ((even, odd): @unchecked) match {
    case (Nil, Nil)             => Nil
    case ((e :: es), (o :: os)) => e :: (o :: mergeEvenOdd(es, os))
  }

  @inlinestatic
  def fft(xs: List[Complex] @inlinestatic): List[Complex] @inlinestatic = xs match {
    case (x :: Nil) => xs
    case _ =>
      val N = xs.length // assume it's a power of two
      val (even0, odd0) = splitEvenOdd(xs)
      val (even1, odd1) = (fft(even0), fft(odd0))
      val (even2, odd2) = (even1 zip odd1).zipWithIndex.map {
        case ((x, y), k) =>
          val z = omega(k, N) * y
          (x + z, x - z)
      } unzip;
      even2 ::: odd2
  }

  // variable argument lists?
  // power function
}*/
