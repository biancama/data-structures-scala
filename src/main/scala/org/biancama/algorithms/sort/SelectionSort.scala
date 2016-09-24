package org.biancama.algorithms.sort

import scala.annotation.tailrec

/**
  * Created by massimo on 30/04/16.
  */
object SelectionSort {
  /**
    * insertsort :: Ord a => [a] -> [a]
    * insertsort la    = insert la []
    * insert :: Ord a => [a] -> [a] -> [a]
    * insert [] la => la
    * insert lu la => insert lu remove min(lu) la add min(lu)
 *
 * @return ordered list
    */
  def sort[A](unsortedList: List[A])(implicit ord: Ordering[A]): List[A] = {
    def remove[A] (la: List[A], elem :A ) = la diff List(elem)

    @tailrec
    def insert[A](unsortedList: List[A], sortedList: List[A])(implicit ord: Ordering[A]): List[A] ={
      if (unsortedList isEmpty) return sortedList
      else {
        val minValue = unsortedList.min
        insert(remove(unsortedList,  minValue), minValue::sortedList)
      }

    }
    insert(unsortedList, List())
  }
}
