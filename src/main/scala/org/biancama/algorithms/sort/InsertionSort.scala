package org.biancama.algorithms.sort

/**
  * Created by massimo on 27/04/16.
  */

object InsertionSort {
  /**
    * insertsort :: Ord a => [a] -> [a]
    * insertsort []    = []
    * insertsort (h:t) = insert h (insertsort t)
    * @return ordered list
    */
  def sort[A](la: List[A])(implicit ord: Ordering[A]): List[A] = {
    la.foldLeft(List[A]())((l, elem) => insert(elem, l))
  }

  /**
    * insert :: Ord a => a -> [a] -> [a]
    * insert item []  = [item]
    * insert item (h:t) | item <= h = item:h:t
    *                   | otherwise = h:(insert item t)
    *
    * @param elem element to insert
    * @param la list already ordered
    * @return ordered list with elem
    */
  def insert[A](elem:A, la: List[A])(implicit ord:Ordering[A]): List[A] ={
    la match {
      case List() => List(elem)
      case head::tail => if (ord.lt(elem, head))  elem::la else head::insert(elem, tail)
    }
  }
}
