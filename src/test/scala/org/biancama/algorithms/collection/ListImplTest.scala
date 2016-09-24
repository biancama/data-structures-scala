package org.biancama.algorithms.collection

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by massimo on 20/05/16.
  */
class ListImplTest extends FlatSpec with Matchers {

  "A list " should "be capable to do some operations" in {
    val ll = ListImpl(1,2,3)
    ll.isEmpty shouldBe false
    ll.length shouldBe 3
    ll(0) shouldBe 1
    ll(1) shouldBe 2
    ll(2) shouldBe 3
    4 +: ll shouldBe ListImpl(4,1,2,3)
    ll :+ 4 shouldBe ListImpl(1,2,3,4)
    val ll1 = ll.filter(x => x % 2 == 0)
    ll1.length shouldBe 1
    ll1(0) shouldBe 2

    val ll2 = ll.filter(x => x % 2 != 0)
    ll2.length shouldBe 2
    ll2(0) shouldBe 1
    ll2(1) shouldBe 3

    ll.equals(ListImpl(1,2,3)) shouldBe true
    ll.equals(ListImpl(3,2,1)) shouldBe false
    ll.foldLeft(0)(_+_) shouldBe 6
    ll.foldLeft(1)(_*_) shouldBe 6
    ll.foldLeft(0)(_*_) shouldBe 0
    ll.foldLeft(ll.head)((_, c) => c) shouldBe 3

    ll.forAll(x=> x % 2 == 0) shouldBe false
    ll.forAll(x=> x < 4 ) shouldBe true

    ll.map(x=> "c" * x ) shouldBe ListImpl("c", "cc", "ccc")
  }



}
