package cernoch.scalistics.aggregator

class StringAggs extends (() => Map[String,Double]) {

  private var _cnt = BigInt(0)

  def apply = Map("COUNT" -> _cnt.toDouble)

  def +=(v: Any) = {
    _cnt = _cnt + 1
  }
}
