package org.biancama.algorithms.sort

import scala.annotation.tailrec

/**
  * Created by massimo on 30/04/16.
  */
object BubbleSort {
  /**
    * bubbleIter :: Ord a => [a] -> [a]
    * bubble la while la isn NOT ordered then bubble la
 *
 * @return ordered list
    */
  def sort[A](unsortedList: List[A])(implicit ord: Ordering[A]): List[A] = {

    /**
      * bubbleIter :: Ord a => [a] -> [a] -> [a]
      * bubbleIter x:y:xs
      * if x > y => y:x::xs
      * else x:y:xs
      *
      * @return ordered list only for the first bubble
      */
    @tailrec
    def bubbleIter(xs: List[A], app: List[A]): List[A] = xs match {
      case y::ys => ys match {
        case z::zs => if ( ord.lt(y,z) ) bubbleIter(zs, List(y, z) ::: app)
        else bubbleIter(zs, List(z, y) ::: app)
        case Nil => y::app
      }
      case Nil => app
    }
    /**
      * at the first bubble iteration the last element is in the right order
      * @param unsorted
      * @param sorted list already sorted
      * @return sorted
      */
    @tailrec
    def sort (unsorted: List[A], sorted: List[A]): List[A] =
      if (unsorted.isEmpty) sorted
      else {
        val firstBubble = bubbleIter(unsorted, List())
        sort (firstBubble.dropRight(1),firstBubble.last::sorted)
      }

    sort(unsortedList, List())
  }
}
