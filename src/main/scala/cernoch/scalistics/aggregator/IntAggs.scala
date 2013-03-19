package cernoch.scalistics.aggregator

import single._

class IntAggs extends (() => Map[String,Double]) {

	private implicit def bigInt2bigDec(l: BigInt) = BigDecimal(l)

	private var _cnt = BigInt(0)
	private var _sum = BigInt(0)

	private val _min    = MIN.forBigInt
	private val _max    = MAX.forBigInt
	private val _median = MEDIAN.forBigInt

	private def _mean = new MEAN(new SUM(BigDecimal(_sum)), new SUM(_cnt))

	def min    = "MIN"    -> _min()
	def max    = "MAX"    -> _max()
	def count  = "COUNT"  -> Some(_cnt)
	def mean   = "MEAN"   -> _mean()
	def median = "MEDIAN" -> _median()
	def mode   = "MODE"   -> new MODE(_median.map).apply()

	def apply()
	= List(min, max, count, mean, median, mode)
		.filter{case (_,o) => o.isDefined}.toMap
		.mapValues{_.get.toDouble}

	def +=(v: BigInt) {
		_cnt = _cnt + 1
		_sum = _sum + v

		_min += v
		_max += v
		_median += v
	}
}
