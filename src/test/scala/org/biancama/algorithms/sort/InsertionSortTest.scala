package org.biancama.algorithms.sort

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by massimo on 27/04/16.
  */
class InsertionSortTest extends FlatSpec with Matchers {
  "A list of integers" should "be orderable" in {
    val unorderedList = List(8, 7, 6, 5, 4, 3, 2, 1)
    val actualList = InsertionSort.sort(unorderedList)
    actualList should have size 8
    actualList should contain theSameElementsAs List(1, 2, 3, 4, 5, 6, 7, 8)
  }

  "A list of repeated integers" should "be orderable" in {
    val unorderedList = List(8, 7, 6, 5, 5, 4, 3, 3, 3, 2, 1)
    val actualList = InsertionSort.sort(unorderedList)
    actualList should have size 11
    actualList should contain theSameElementsAs List(1, 2, 3, 3, 3, 4, 5, 5, 6, 7, 8)
  }

  "An insert in a empy List" should "list with one element" in {
    val actualList = InsertionSort.insert(1, List())
    actualList should contain theSameElementsAs List(1)
  }
  "An insert in a ordered List" should "be done in correct place" in {
    val actualList = InsertionSort.insert(4, List(1, 2, 5, 6))
    actualList should contain theSameElementsAs List(1, 2, 4, 5, 6)
  }
}
