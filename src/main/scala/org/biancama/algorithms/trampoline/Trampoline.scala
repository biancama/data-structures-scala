package org.biancama.algorithms.trampoline

import scala.annotation.tailrec
import scala.util.control.TailCalls._

object Trampoline {

  private def fib1(n: Int): TailRec[Int] = if (n < 2) done(n) else {
    tailcall(fib1(n-2)).flatMap(x => tailcall(fib1(n-1)).map(y => x + y))
  }

  private def fib2(n : Int): TailRec[Int] = if (n < 2) done(n) else for {
    x <- tailcall(fib2(n - 1))
    y <- tailcall(fib2(n - 2))
  } yield (x + y)


  def fib(n: Int): Int = fib2(n).result

  def fibNaive(n: Int): Int =  if (n < 2) n else fibNaive(n-1) + fibNaive(n-2)
}
