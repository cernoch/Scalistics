package cernoch.scalistics

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
class Bag[T](val data: Map[T, BigInt])
  extends Iterable[T]
    with Function[T, BigInt]
    with ProbabilityMassFunction[T] { hist =>

  def this() = this(Map[T, BigInt]())

  // === READ OPERATIONS ================================================== //

  /** Returns the frequency of the value */
  def apply(t: T) = data.get(t).getOrElse(BigInt(0))

  /**Iterates over all values on the bag */
  def iterator = data.keys.iterator

  /**Sum of all values' counts */
  def sum
  = if (sumCache != null)
    sumCache
  else {
    sumCache = sumCount
    sumCache
  }

  private var sumCache: BigInt = null

  private def sumCount = data.values.foldLeft(BigInt(0)) {
    _ + _
  }

  // === “WRITE” OPERATIONS =============================================== //

  /**
   * Increases the frequency of an value by 1
   *
   * This bag is immutable, so the properties of `this` remain unchanged.
   *
   * @param t Value whose frequency will be raised
   * @return New bag with the increased value.
   */
  def inc(t: T) = add(t, 1)

  /**
   * Increases the frequency of an value by an arbitrary value
   *
   * This bag is immutable, so the properties of `this` remain unchanged.
   *
   * @param t Value whose frequency will be raised
   * @return New bag with the increased value.
   */
  def add(t: T, n: BigInt) = new Bag[T](data + ((t, this(t) + n)))

  /**
   * Merges `this` with the given bag of values
   *
   * This bag is immutable, so the properties of `this` remain unchanged.
   *
   * @return New bag with the increased value.
   */
  def ++(u: Bag[T]) = u.data.foldLeft(this)(
    (prev, elem) => prev.add(elem._1, elem._2))

  // === VIEWS ============================================================ //

  /**
   * Probability mass of the given value
   *
   * If `this` was taken as a probability distribution on the values,
   * returns probability mass of the supplied value.
   *
   * @return Value between 0 and 1
   */
  def mass(t: T) = BigDecimal(apply(t)) / BigDecimal(sum)



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

  override def toString = toStrBuilder(new StringBuilder).toString

  override def hashCode = data.hashCode()

  override def equals(o: Any) = o match {
    case h: Bag[_] => data == h.data
    case _ => false
  }
}


object Bag {

  /**Very inefficient way of creating a bag */
  def apply[T]
  (i: Iterable[T])
  = i.foldLeft(
    new Bag[T]()
  )(
    (h, e) => h.inc(e)
  )
}