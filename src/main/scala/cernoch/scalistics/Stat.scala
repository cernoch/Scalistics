package cernoch.scalistics

trait CumulativeDistribution[T] {

  def distrib
  (i: Interval[T, Open[T], Closed[T]])
  (implicit v: (T) => Ordered[T])
  : BigDecimal
}

trait ProbabilityMassFunction[T] {

  def mass(i: T): BigDecimal
}

object BigDecStat {

  def max[T](i: Iterable[BigDecimal])
  = min(i.map {
    -_
  }) match {
    case Some(v) => Some(-v)
    case None => None
  }

  def min[T](i: Iterable[BigDecimal])
  = if (i.size == 0) None
  else
    Some(i.reduce {
      (a, b) => if (a < b) a else b
    })

  def mean[T](i: Iterable[BigDecimal])
  = if (i.size == 0) None
  else
    Some(i.foldLeft(BigDecimal(0)) {
      _ + _
    } / BigDecimal(i.size))

  def median[T](i: Iterable[BigDecimal])
  = if (i.size == 0) None
  else
    Some(i.toList.sortWith {
      _ < _
    }(i.size / 2))
}