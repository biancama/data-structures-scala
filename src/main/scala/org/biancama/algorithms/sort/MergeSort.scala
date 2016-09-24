package org.biancama.algorithms.sort

import scala.annotation.tailrec

/**
  * Created by massimo on 30/04/16.
  */
object MergeSort {
  /**
    *  mergesort :: Ord a => [a] -> [a]
    *  mergesort [] = []
    *  mergesort [x] = [x]
    *  mergesort xs = merge (mergesort (xs take n)) (mergesort (xs drop n))
    *
    * @return ordered list
    */
  def sort[A](unsortedList: List[A])(implicit ord: Ordering[A]): List[A] = {
    /**
      *  merge :: Ord a => [a] -> [a] -> [a]
      *  merge xs [] = xs
      *  merge [] ys = ys
      *  merge (x:xs) (y:ys)
      *      | (x <= y)  = x:(merge xs (y:ys))
      *      | otherwise = y:(merge (x:xs) ys)
      *
      * @return ordered list
      */
    @tailrec
    def merge (ll :List[A], lr: List[A], app: List[A]) : List[A] = (ll, lr) match {
      case (Nil, Nil) => app
      case (_, Nil) => ll ++ app
      case (Nil, _) => lr ++ app
      case (xl::xxl, xr::xxr) => if(ord.lt(xl, xr)) merge(xxl, lr, xl::app) else merge(ll, xxr, xr::app)
    }
    if (unsortedList.length < 2) unsortedList
    else {
      val center = unsortedList.length / 2
      val (left, right) = unsortedList splitAt(center)
      merge(sort(left), sort(right), Nil)
    }
  }
}
