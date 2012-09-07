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
 * One-dimensional convex set of values
 *
 * Interval is a set of values defined by its left and right border.
 * Any value between the two borders is included in the interval.
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Interval
  [T, +L <: Endpoint[T],
      +R <: Endpoint[T]]
  (val l: L, val r: R)
  (implicit f: Basis[T]) {

  /**Is a value member of this interval? */
  def contains(value: T) = {
    val v = new Endpoint[T]() {
      def get = Some(value)
    }
    f.lt(l, v) && f.lt(v, r)
  }

  def meets(i: Interval[T, Endpoint[T], Endpoint[T]])
  = f.equiv(r, i.l)

  def <(i: Interval[T, Endpoint[T], Endpoint[T]])
  = f.lt(r, i.l)

  def >(i: Interval[T, Endpoint[T], Endpoint[T]])
  = f.lt(i.r, l)

  /*
  def overlaps(i : Interval[T,Endpoint[T],Endpoint[T]])
    = f().gt(r, i.l) && f().lt(l, i.r)

  def starts(i : Interval[T,Endpoint[T],Endpoint[T]])
    = f().equiv(l, i.l)

  def during(i : Interval[T,Endpoint[T],Endpoint[T]])
    = f().gt(l, i.l) && f().lt(r, i.r)

  def finishes(i : Interval[T,Endpoint[T],Endpoint[T]])
    = f().equiv(r, i.r)
  */

  /**Intersection with another interval */
  def intersect
    [LO >: L <: Endpoint[T],
     RO >: R <: Endpoint[T]]
    (i: Interval[T,LO,RO])
  = new Interval[T,LO,RO](
      l max i.l,
      r min i.r)

  override def toString = l.toStrBeg + ".." + r.toStrEnd
  override def hashCode = l.hashCode + 3 * r.hashCode
  override def equals(o:Any) = o match {
    case i:Interval[_,_,_] => (l == i.l) && (r == i.r)
    case _ => false
  }
}


object Interval {

  def apply
    [T, L <: Endpoint[T], R <: Endpoint[T]]
    (l: L, r: R)
    (implicit base: Basis[T])
  = new Interval[T, L, R](l, r)


  def empty[T]
    (implicit base: Basis[T])
  = Interval[T,
    PosInfty[T], NegInfty[T]](
    PosInfty[T], NegInfty[T])

  def entire[T]
    (implicit base: Basis[T])
  = Interval[T,
    NegInfty[T], PosInfty[T]](
    NegInfty[T], PosInfty[T])

  def opened[T]
    (l: T, r: T)
    (implicit base: Basis[T])
  = Interval[T,
    Open[T], Open[T]](
    Open(l), Open(r))

  def closed[T]
    (l: T, r: T)
    (implicit base: Basis[T])
  = Interval[T,
    Closed[T], Closed[T]](
    Closed(l), Closed(r))


  def openClosed[T]
    (l: T, r: T)
    (implicit base: Basis[T])
  = Interval[T,
    Open[T], Closed[T]](
    Open(l), Closed(r))

  def closedOpen[T]
    (l: T, r: T)
    (implicit base: Basis[T])
  = Interval[T,
    Closed[T], Open[T]](
    Closed(l), Open(r))


  def fromOpened[T](t: T)(implicit base: Basis[T])
  = Interval[T, Open[T], PosInfty[T]](Open(t), PosInfty[T]())

  def fromClosed[T](t: T)(implicit base: Basis[T])
  = Interval[T, Closed[T], PosInfty[T]](Closed(t), PosInfty[T]())


  def toOpened[T](t: T)(implicit base: Basis[T])
  = Interval[T, NegInfty[T], Open[T]](NegInfty[T](), Open(t))

  def toClosed[T](t: T)(implicit base: Basis[T])
  = Interval[T, NegInfty[T], Closed[T]](NegInfty[T](), Closed(t))
}
