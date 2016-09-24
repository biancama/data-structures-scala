package org.biancama.algorithms.sort

/**
  * Created by massimo on 30/04/16.
  */
object QuickSort {
  /**
    *  quicksort :: (Ord a) => [a] -> [a]
    *  quicksort [] = []
    *  quicksort (x:xs) =
    *    let smallerSorted = quicksort [a | a <- xs, a <= x]
    *        biggerSorted = quicksort [a | a <- xs, a > x]
    *      in  smallerSorted ++ [x] ++ biggerSorted
    *
    * @return ordered list
    */
  def sort[A](unsortedList: List[A])(implicit ord: Ordering[A]): List[A] = {
    def getPivot(la: List[A]) = {
      val center = la(la.size / 2)
      val left = la(0)
      val right = la(la.size - 1)
      if (ord.compare(left, center) <= 0 && ord.compare(center, right) <= 0 || ord.compare(left, center) >= 0 && ord.compare(center, right) >= 0 ) {
        center
      } else if (ord.compare(center, left) <= 0 && ord.compare(left, right) <= 0 || ord.compare(center, left) >= 0 && ord.compare(left, right) >= 0 ) {
        left
      } else {
        right
      }
    }

    def partition (la: List[A], pivot: A) = (la.filter(x => ord.compare(x, pivot) < 0), la.filter(x => ord.compare(pivot, x) < 0), la.filter(x => ord.compare(pivot, x) == 0))
    if (unsortedList.isEmpty || unsortedList.size == 1) unsortedList else {
      val pivot = getPivot(unsortedList)
      val (less, more, equal) = partition(unsortedList, pivot)
      sort(less) ::: equal ::: sort(more)
    }
  }
}
