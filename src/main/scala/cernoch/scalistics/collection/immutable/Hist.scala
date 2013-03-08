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

import collection.immutable.TreeMap
import collection.mutable.ArrayBuffer
import cernoch.scalistics.interval._


/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Hist[T: Numeric, N: Numeric]
	(val cutPoints: TreeMap[T,N],
 	 val above:N)
	extends (T => N) {

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
	def sum = {
		val _i = implicitly[Numeric[N]]; import _i._

		cutPoints.values.foldLeft(above){plus}
	}

	def max = {
		val _i = implicitly[Numeric[N]]; import _i._

		var max = above
		for (v <- cutPoints.valuesIterator)
			if (lt(max,v)) max = v
		max
	}

	/**Converts the cutPoints into the list of binCenters */
	def binCentres()(implicit int: Fractional[T]) = {
		def div(a:T,b:T): T = int.div(
			int.plus(a,b),
			int.plus(int.one,int.one)
		)
		binCenters(div)
	}

	/**Converts the cutPoints into the list of binCenters */
	def binCenters(midPoint: (T, T) => T)
	= {
		val _i = implicitly[Numeric[T]]; import _i._

		cutPoints.size match {

			case 0 => List(implicitly[Numeric[T]].zero)
			case 1 => List(
				minus(cutPoints.keys.head, one),
				plus(cutPoints.keys.head,  one))
			case _ => {
				var binCenters = new ArrayBuffer[T]()

				var first = true
				var last: T = zero
				for (curr <- cutPoints.keys) {

					if (!first)
						binCenters = binCenters :+ midPoint(last, curr)

					first = false
					last = curr
				}

				if (!binCenters.isEmpty) {
					val lastGap = minus(cutPoints.keys.last, binCenters.last)
					val headGap = minus(binCenters.head, cutPoints.keys.head)

					binCenters = binCenters :+ plus(cutPoints.keys.last, lastGap)
					binCenters = minus(cutPoints.keys.head, headGap) +: binCenters
				}

				binCenters.toList
			}
		}}


	/**Convert this set to intervals */
	def intervals
	: List[Interval[T, Open[T], Closed[T]]]
	= {
		val b = new Basis(implicitly[Numeric[T]])
		if (cutPoints.isEmpty)
			List(Interval.entire[T](b))
		else
			Interval.toClosed[T](cutPoints.keySet.head)(b) ::
				intervalize(cutPoints.keySet.toList, b)
	}

	private def intervalize
	(l: List[T], basis: Basis[T])
	: List[Interval[T, Open[T], Closed[T]]]
	= l match {

		case h1 :: h2 :: tail
		=> Interval.openClosed(h1, h2)(basis) :: intervalize(h2 :: tail, basis)

		case head :: Nil
		=> List(Interval.fromOpened(head)(basis))

		case Nil => throw new IllegalArgumentException(
			"Cannot create intervals from an empty set")
	}


	// === “WRITE” OPERATIONS =============================================== //

	/**
	 * Increases the frequency of an value by an arbitrary value
	 *
	 * @param t Value whose bin's frequency will be raised
	 * @return New bag with the increased value.
	 */
	def +(t: T, n: N = implicitly[Numeric[N]].one) = {
		val _i = implicitly[Numeric[N]]; import _i._

		val proj = cutPoints.keySet.from(t)
		if (proj.isEmpty)
			new Hist[T,N](cutPoints, above + n)
		else
			new Hist[T,N](
				cutPoints + (proj.head -> plus(cutPoints(proj.head), n)),
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

	override def toString() = toStrBuilder(new StringBuilder).toString()

	override def hashCode() = cutPoints.hashCode() + above.hashCode()

	override def equals(o: Any) = o match {
		case h: Hist[_,_] => cutPoints == h.cutPoints && above == h.above
		case _ => false
	}
}


object Hist {

	def small[T:Numeric]
	(binCentr: Iterable[T])
	(values:   Iterable[T])
	= values.foldLeft(new Hist[T,Int](
		new TreeMap() ++ binCentr.map{_ -> 0}, 0)
	){_ + _}

	def large[T:Numeric]
	(binCentr: Iterable[T])
	(values:   Iterable[T])
	= values.foldLeft(new Hist[T,Int](
		new TreeMap() ++ binCentr.map{_ -> 0}, 0)
	){_ + _}

	def apply[T:Integral](bins: Int)
	(values: Iterable[T])
	= {
		val _i = implicitly[Integral[T]]; import _i._

		def decList(togo:Int) : List[T]
		= if (togo > 0)
			List(one)
		else decList(togo-1) match {
			case head :: tail
			=> plus(one,head) :: head :: tail
		}

		decList(bins)
	}
}
