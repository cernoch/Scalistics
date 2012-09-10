package cernoch.scalistics

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import collection.immutable.TreeMap

@RunWith(classOf[JUnitRunner])
class HistTest extends Specification {

  "Histogram" should {

    val hist = new Hist[Int](TreeMap(0->1, 10->2), 3)

    "be created from the apply function" in {
      Hist(1 to 10, List(3,6)) must_==
        new Hist[Int](TreeMap(3->3, 6->3), 4)
    }

    "have no cutpoint if the apply receives empty number of cutPoints" in {
      Hist(1 to 10, List()) mustEqual new Hist[Int](TreeMap(), 10)
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

    val hist = Hist(1 to 10, List(3,6))
    val centres = hist.binCenters{(x,y) => (x+y)/2}

    "be created correctly" in {
      val midPoint = (3+6)/2
      val leftPoint = 3+3 - midPoint
      val rightPoint = 6+6 - midPoint

      centres must_== List(leftPoint, midPoint, rightPoint)
    }

    "throw an exception on empty hist" in {
      Hist(1 to 10, List()).binCenters{(x,y) => (x+y)/2} mustEqual
        List(0)
    }
  }
}
