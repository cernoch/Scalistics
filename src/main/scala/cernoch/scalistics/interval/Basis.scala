package cernoch.scalistics.interval

/*
 * Copyright (c) 2012 Radomír Černoch (radomir.cernoch at gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Basis[T](val ord: Ordering[T])
  extends Ordering[Endpoint[T]] {

  def negInfty = NegInfty[T]()

  def posInfty = PosInfty[T]()

  def this(comp: (T, T) => Int) = this(Measure[T](comp))

  def compare(x: Endpoint[T], y: Endpoint[T]) = (x, y) match {
    case (NegInfty(), NegInfty()) => 0
    case (PosInfty(), PosInfty()) => 0

    case (NegInfty(), _) => -1
    case (_, PosInfty()) => -1
    case (PosInfty(), _) => +1
    case (_, NegInfty()) => +1

    case (Endpoint(a), Endpoint(b))
    => ord.compare(a.get, b.get)
  }

}

class Measure
[T, M]
(ord: Ordering[T], distance: (T, T) => M)
  extends Basis[T](ord) {

  def this(comp: (T, T) => Int, distance: (T, T) => M)
  = this(Measure(comp), distance)

  def dist(a: T, b: T) = distance(a, b)
}

object Measure {

  def apply[T]
  (f: (T, T) => Int)
  = new Ordering[T] {
    def compare(x: T, y: T) = f(x, y)
  }
}

object Basis {

  implicit object INT_BASIS extends
  Measure[Int, Int](
    (a: Int, b: Int) => a - b,
    (a: Int, b: Int) => scala.math.abs(a - b)
  ) {}


  //implicit def floatBasis() = FLOAT_BASIS
  implicit object FLOAT_BASIS
    extends Measure[Float, Float](
      (a, b) => scala.math.signum(a - b).round,
      (a, b) => scala.math.abs(a - b)
    ) {}

  //implicit def doubleBasis() = DOUBLE_BASIS
  implicit object DOUBLE_BASIS
    extends Measure[Double, Double](
      (a, b) => scala.math.signum(a - b).round.toInt,
      (a, b) => scala.math.abs(a - b)
    ) {}

  //implicit def bigIntBasis() = BIGINT_BASIS
  implicit object BIGINT_BASIS
    extends Measure[BigInt, BigInt](
      (a, b) => (a - b).signum,
      (a, b) => (a - b).abs
    ) {}

  //implicit def bigDecimalBasis() = BIGDECIMAL_BASIS
  implicit object BIGDECIMAL_BASIS
    extends Measure[BigDecimal, BigDecimal](
      (a, b) => (a - b).signum,
      (a, b) => (a - b).abs
    ) {}

}
