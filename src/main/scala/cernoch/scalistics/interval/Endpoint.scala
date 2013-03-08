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
 * Endpoint is a left or a right border of an [[Interval]]
 *
 * We distinguish the ''values'' (e.g. -1, 10.3, +∞) and the corresponding
 * ''endpoints'' (e.g. Open(-1), Closed(10.3), PosInfty).
 *
 * Since some domains do not have values for positive or negative infinity
 * (e.g. Int, BigDecimal), the endpoint returns [[scala.Option]]. The
 * endpoits for negative or positive infinity may return [[scala.None]],
 * while all others must return [[scala.Some]].
 *
 * @tparam T Type of the encapsulated value
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
trait Endpoint[T] {

  /**
   * The encapsulated value.
   *
   * Since some domains do not have values for positive or negative infinity
   * (e.g. Int, BigDecimal), the endpoint returns [[scala.Option]]. The
   * endpoits for negative or positive infinity may return [[scala.None]],
   * while all others must return [[scala.Some]].
   *
   * @return
   */
  def get: Option[T]

  /**
   * Returns the smaller value of `this` and the argument
   *
   * @param that Compared endpoint
   * @tparam X Type of the compared endpoint
   * @return The smaller of two values
   */
  def min
  [X >: this.type <: Endpoint[T]]
  (that: X)
  (implicit base: Basis[T])
  : X
  = if (base.compare(this, that) < 0)
    this
  else that

  /**
   * Returns the larger value of `this` and the argument
   *
   * @param that Compared endpoint
   * @tparam X Type of the compared endpoint
   * @return The smaller of two values
   */
  def max
  [X >: this.type <: Endpoint[T]]
  (that: X)
  (implicit base: Basis[T])
  : X
  = if (base.compare(this, that) > 0)
    this
  else that

  /**Symbol for printing the endpoint if it is the left one */
  private[scalistics] def toStrBeg = "<" + get

  /**Symbol for printing the endpoint if it is the right one */
  private[scalistics] def toStrEnd = get + ">"

  override def toString = "Endpoint(" + get + ")"

  override def hashCode = get.map {
    _.hashCode
  }.getOrElse(0)

  override def equals(o: Any) = o match {
    case Endpoint(got) => get == got
    case _ => false
  }
}

object Endpoint {
  /**Extracts the [[scala.Option]] from the endpoint. */
  def unapply[T](t: Endpoint[T]) = Some(t.get)
}


/**
 * Endpoint of an [[Interval]], which is not its member
 *
 * For example the interval with open endpoints (2,5) contains values 3 and 4.
 *
 * @tparam T Type of the interval values
 */
trait Open[T] extends Endpoint[T] {
  override def toStrBeg = "(" + get.getOrElse(null)

  override def toStrEnd = get.getOrElse(null) + ")"

  override def toString = "Open(" + get.getOrElse(null) + ")"
}

object Open {
  def apply[T](t: T)
  = new Open[T]() {
    def get = Some(t)
  }

  def unapply[T](t: Endpoint[T])
  = if (t.isInstanceOf[Open[T]])
    Some(t.get)
  else None
}


/**
 * Endpoint of an [[Interval]], which is its member
 *
 * For example the interval with closed endpoints [1,3]
 * contains values 1, 2 and 3.
 *
 * @tparam T Type of the interval values
 */
trait Closed[T] extends Endpoint[T] {
  override def toStrBeg = "[" + get.getOrElse(null)

  override def toStrEnd = get.getOrElse(null) + "]"

  override def toString = "Closed(" + get.getOrElse(null) + ")"
}

object Closed {
  def apply[T](t: T)
  = new Closed[T]() {
    def get = Some(t)
  }

  def unapply[T](t: Endpoint[T])
  = if (t.isInstanceOf[Closed[T]])
    Some(t.get.get)
  else None
}


/**
 * Negative infinity as an endpoint for an
 * [[Interval]]
 *
 * @tparam T Type of the interval values
 */
trait NegInfty[T] extends Open[T] with Closed[T] {
  override def toStrBeg = "(" + toString

  override def toStrEnd = toString + ")"

  override def toString = "-∞"

  override def hashCode = -1

  override def equals(o: Any)
  = o.isInstanceOf[NegInfty[T]]
}

object NegInfty {
  def apply[T]()
  = new NegInfty[T]() {
    def get = None
  }

  def apply[T](negInfty: T)
  = new NegInfty[T]() {
    def get = Some(negInfty)
  }

  def unapply[T](t: Endpoint[T])
  = t.isInstanceOf[NegInfty[T]]
}


/**
 * Positive infinity as an endpoint for an [[Interval]]
 *
 * @tparam T Type of the interval values
 */
trait PosInfty[T] extends Open[T] with Closed[T] {
  override def toStrBeg = "(" + toString

  override def toStrEnd = toString + ")"

  override def toString = "+∞"

  override def hashCode = +1

  override def equals(o: Any)
  = o.isInstanceOf[PosInfty[T]]
}

object PosInfty {
  def apply[T]()
  = new PosInfty[T]() {
    def get = None
  }

  def apply[T](posInfty: T)
  = new PosInfty[T]() {
    def get = Some(posInfty)
  }

  def unapply[T](t: Endpoint[T])
  = t.isInstanceOf[PosInfty[T]]
}
