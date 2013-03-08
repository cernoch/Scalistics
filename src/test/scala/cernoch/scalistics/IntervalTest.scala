package cernoch.scalistics.interval

import Interval._
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IntervalTest extends Specification {

  "Interval intersection" should {

    val oneThree = closedOpen(1,3);
    val twoFour  = closedOpen(2,4);

    "be type-closed" in {
      val i: Interval[Int,Closed[Int],Open[Int]]
        = oneThree intersect twoFour
      true
    }

    "compute correct values for integers" in {
      val i = oneThree intersect twoFour
      i must_== closedOpen(2,3)
    }
  }



  "Meets operation" should {
    import Basis.FLOAT_BASIS

    "be true if right meets the left" in {
      closedOpen(2,4) meets closedOpen(4,5)
    }

    "be false if left meets the right" in {
      !( closedOpen(4,5) meets closedOpen(2,4) )
    }

    "be true if empty meets entire" in {
      Interval.empty meets Interval.entire
    }

    "be false if entire meets entire" in {
      !( Interval.entire meets Interval.entire )
    }
  }



  "Interval comparison" should {
    "work for strictly ordered intervals" in {
      opened(1,2) < opened(4,5)
    }

    "work for overlapping intervals" in {
      !( opened(1,3) < opened(2,5) )
    }
  }



  "Interval meeting" should {
    "give true if two share mutual border" in {
      opened(2,4.5) meets opened(4.5,8)
    }

    "give false if disjoint" in {
      !( opened(2,4) meets opened(8,10) )
    }

    "give false if overlap" in {
      !( opened(2,5) meets opened(4,6) )
    }
  }
}