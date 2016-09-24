package org.biancama.algorithms.stack

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by massimo on 24/09/16.
  */
class StackImplTest extends FlatSpec with Matchers {
  "A stack " should "be capable to do some operations" in {
    val s = Stack[Int]()
    s.isEmpty shouldBe true
    val s1 = s.push(2)
    val s2 = s1.push(1)
    s2.isEmpty shouldBe false
    s2.top shouldBe 2
    val s3 = s2.pop
    s3.top shouldBe 1
    val s4 =s3.pop
    s4.isEmpty shouldBe true


  }
}
