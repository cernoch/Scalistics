package cernoch.scalistics.collection.immutable

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
 * Stores frequency counts in a bag of values
 *
 * @param data Internal map to store the frequencies
 * @tparam T Type of the stored elements
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Bag[T,N:Numeric]
	(val data: Map[T,N] = Map.empty[T,N])
	extends Iterable[T]
	with (T => N)
{ hist =>

	private val _implicit = implicitly[Numeric[N]]
	import _implicit._

  // === READ OPERATIONS ================================================== //

  /** Returns the frequency of the value */
  def apply(t: T) = data.getOrElse(t,zero)

  /**Iterates over all values on the bag */
  def iterator = data.keys.iterator

  /**Sum of all values' counts */
  lazy val sum = data.values.foldLeft(zero)(plus)

	// === “WRITE” OPERATIONS =============================================== //

	def +(t:T, n:N = one) = new Bag[T,N](data + (t -> plus(this(t),n)))

	def ++(u: Iterable[T]) = u.foldLeft(this){_ + _}

	def ++(u: Bag[T,N]) = u.data.foldLeft(this){
		case (bag, (value, count)) => bag.+(value, count)
	}

	// === IMPLEMENTATION GARBAGE =========================================== //

  /**Converts this to a string builder */
  def toStrBuilder(s: StringBuilder) = {
		s append "Bag("
    var first = true
    for (t <- data) {
      if (!first) s append ", "
      s append t._1 append " -> " append t._2
      first = false
    }
    s append ")"
  }

  override def toString() = toStrBuilder(new StringBuilder).toString()

  override def hashCode() = data.hashCode()

  override def equals(o: Any) = o match {
    case h: Bag[_,_] => data == h.data
    case _ => false
  }
}

object Bag {
	def small[T](i: Iterable[T]) = new Bag[T,Int]() ++ i
	def large[T](i: Iterable[T]) = new Bag[T,BigInt]() ++ i
}