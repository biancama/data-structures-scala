package org.biancama.algorithms.trampoline

import org.scalatest.{FlatSpec, Matchers}

class TrampolineTest extends FlatSpec with Matchers {
  "Fibonacci of n " should "return correct calculation" in {
    Trampoline.fib(6) shouldBe 8
    Trampoline.fib(7) shouldBe 13
    Trampoline.fib(40) shouldBe 102334155
  }

  "Fibonacci naive of n " should "throw stack overflow" in {
    Trampoline.fibNaive(40) shouldBe 102334155
  }
}
