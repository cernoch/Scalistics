package cernoch.scalistics.collection.immutable

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import collection.immutable.TreeMap

@RunWith(classOf[JUnitRunner])
class HistTest extends Specification {

  "Histogram" should {

    val hist = new Hist(TreeMap(0->1, 10->2), 3)

    "be created from the apply function" in {
      Hist.int(List(3,6))(1 to 10) must_==
        new Hist(TreeMap(3->3, 6->3), 4)
    }

    "have no cut-point if the empty number of cut-points given" in {
      Hist.int(List[Int]())(1 to 10) mustEqual
				new Hist(TreeMap[Int,Int](), 10)
    }

    "represent interior values correctly" in {
      (hist(-1) must_== 1) &&
      (hist( 5) must_== 2) &&
      (hist(15) must_== 3)
    }

    "represent values at cut-points correctly" in
      { (hist( 0) must_== 1) && (hist(10) must_== 2) }

    "increase the left-most bin correctly" in
      { (hist + 0)(-1) must_== 2 }

    "increse the middle bin correctly" in
      { ((hist + 5)(5) must_== 3) && ((hist + 10)(10) must_== 3) }

    "increase the right-most bin correctly" in
      { (hist + 11)(11) must_== 4 }
  }



  "Bin centres" should {

    val hist = Hist.int(List(3,6))(1 to 10)
    val centres = hist.binCenters((a,b) => (a + b) / 2)

    "be created correctly" in {
      val midPoint = (3+6)/2
      val leftPoint = 3+3 - midPoint
      val rightPoint = 6+6 - midPoint

      centres must_== List(leftPoint, midPoint, rightPoint)
    }

    "throw an exception on empty hist" in {
      Hist.int(List[Double]())(1 to 10 map {_.toDouble})
				.binCenters((a,b) => (a + b) / 2) mustEqual List(0)
    }
  }
}
