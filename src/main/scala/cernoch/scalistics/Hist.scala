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

import collection.immutable.TreeMap
import collection.mutable.LinkedList


/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Hist
    [T]
    (val cutPoints: TreeMap[T, BigInt], val above: BigInt)
    (implicit num: Numeric[T])
  extends Function[T, BigInt] {
    //with CumulativeDistribution[T] {

  // === READ OPERATIONS ================================================== //

  /** Returns the frequency of the value */
  def apply(t: T) = {
    val proj = cutPoints.keySet.from(t)

    if (proj.isEmpty)
      above
    else
      cutPoints(proj.head)
  }

  /**Sum of all values' counts */
  def sum
  = if (sumCache != null)
    sumCache
  else {
    sumCache = sumCount
    sumCache
  }

  private var sumCache: BigInt = null

  private def sumCount = cutPoints.values.foldLeft(above) {
    _ + _
  }

  /**Converts the cutPoints into the list of binCenters */
  def binCenters
    (midPoint: (T, T) => T)
  = cutPoints.size match {
    case 0 => List(num.zero)
    case 1 => List(
      num.minus(cutPoints.keys.head, num.one),
      num.plus(cutPoints.keys.head, num.one))
    case _ => {
      var binCenters = new LinkedList[T]()

      var first = true
      var last: T = num.zero
      for (curr <- cutPoints.keys) {

        if (!first)
          binCenters = binCenters :+ midPoint(last, curr)

        first = false
        last = curr
      }

      if (!binCenters.isEmpty) {
        val lastGap = num.minus(cutPoints.keys.last, binCenters.last)
        val headGap = num.minus(binCenters.head, cutPoints.keys.head)

        binCenters = binCenters :+ num.plus(cutPoints.keys.last, lastGap)
        binCenters = num.minus(cutPoints.keys.head, headGap) +: binCenters
      }

      binCenters.toList
    }
  }


  /**Convert this set to intervals */
  def intervals
    (implicit basis: Basis[T])
  : List[Interval[T, Open[T], Closed[T]]]
  = if (cutPoints.isEmpty)
      List(Interval.entire[T])
    else
      Interval.toClosed[T](cutPoints.keySet.head) ::
        intervalize(cutPoints.keySet.toList)

  private def intervalize
    (l: List[T])
  : List[Interval[T, Open[T], Closed[T]]]
  = l match {

    case h1 :: h2 :: tail
      => Interval.openClosed(h1, h2)(new Basis(num)) :: intervalize(h2 :: tail)

    case head :: Nil
      => List(Interval.fromOpened(head)(new Basis(num)))

    case Nil
      => throw new IllegalArgumentException(
        "Cannot create intervals from an empty set")
  }


  // === “WRITE” OPERATIONS =============================================== //

  /**
   * Increases the frequency of an value by 1
   *
   * This hist is immutable, so the properties of `this` remain unchanged.
   *
   * @param t Value whose bin's frequency will be raised
   * @return New bag with the increased value.
   */
  def +(t: T) = add(t,BigInt(1))

  /**
   * Increases the frequency of an value by an arbitrary value
   *
   * This hist is immutable, so the properties of `this` remain unchanged.
   *
   * @param t Value whose bin's frequency will be raised
   * @return New bag with the increased value.
   */
  def add(t: T, n: BigInt) = {

    val proj = cutPoints.keySet.from(t)
    if (proj.isEmpty)
      new Hist[T](cutPoints, above + n)
    else
      new Hist[T](
        cutPoints + ((proj.head, cutPoints(proj.head) + n)),
        above)
  }

  // === IMPLEMENTATION GARBAGE =========================================== //

  /**Converts this to a string builder */
  def toStrBuilder(s: StringBuilder) = {
    s append "Hist("
    for (t <- cutPoints)
      s append t._2 append " {" append t._1 append "} "
    s append above
    s append ")"
  }

  override def toString = toStrBuilder(new StringBuilder).toString

  override def hashCode = cutPoints.hashCode() + above.hashCode()

  override def equals(o: Any) = o match {
    case h: Hist[_] => cutPoints == h.cutPoints && above == h.above
    case _ => false
  }
}


object Hist {

  def apply
    [T]
    (values: Iterable[T], binCentr: Iterable[T])
    (implicit numeric: Numeric[T])
  = values.foldLeft(
    new Hist[T](
      new TreeMap() ++ binCentr.map{(_, BigInt(0))}, BigInt(0))
    ){ _ + _ }

  def apply
    [T]
    (values: Iterable[T], bins:Int)
    (implicit numeric: Integral[T])
  = {
    import numeric._

    val minVal = values.reduce(min(_,_))
    val maxVal = values.reduce(max(_,_))

    val step = numeric.minus(maxVal, minVal)

    def decList
      (togo:Int)
    : List[T]
    = if (togo > 0)
        List(one)
      else decList(togo-1) match {
        case head :: tail
          => plus(one,head) :: head :: tail
      }

    val largest :: others = decList(bins)
    
    decList(bins)
  }


  def main(args: Array[String]) : Unit = {

    println(Hist(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 4))
  }
}
