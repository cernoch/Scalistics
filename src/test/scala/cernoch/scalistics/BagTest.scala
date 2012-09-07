package cernoch.scalistics

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import Interval._

@RunWith(classOf[JUnitRunner])
class BagTest extends Specification {

  "Bag of integers" should {

    val lst = List(1,2,2,2,2,3,3,3,3,3,3,4,4,4,4,5)
    val bag = Bag(lst)

    "be created using 'apply' correctly" in {
      bag == new Bag(Map(1->1, 2->4, 3->6, 4->4, 5->1))
    }

    "remember the frequency counts" in {
      (bag(1) must_== 1) && (bag(3) must_== 6)
    }

    "sum the values correctly" in {
      bag.sum must_== 16
    }

    "compute the probability mass" in {
      bag.mass(2) must_== BigDecimal(4) / 16
    }

    "append one element" in {
      bag inc 8 must_== Bag(8 :: lst)
    }

    "increase an element by arbitrary number" in {
      bag.add(8,3) must_== Bag(8 :: 8 :: 8 :: lst)
    }

    "merge with another bags" in {
      bag ++ Bag(List(1,2,3)) must_== Bag(1::2::3::lst)
    }
  }
}