package org.biancama.algorithms.collection

import scala.annotation.tailrec

/**
  * Created by massimo on 20/05/16.
  */
abstract sealed class ListImpl[+A] {
  def head: A

  def tail:ListImpl[A]

  def isEmpty:Boolean

  /**
    *  A copy of the list with an element prepended.
    */
  def +:[B >: A](elem: B) : ListImpl[B] = ConsImpl(elem, this)
  /**
    *  A copy of the list with an element appended.
    */
  def :+[B >: A](elem: B): ListImpl[B] = {
    @tailrec
    def concat(ll: ListImpl[B], app: ListImpl[B]): ListImpl[B] = if (ll.isEmpty) app else concat(ll.tail, ll.head +: app)
    concat(this.reverse, ConsImpl(elem, Nil))
  }

  def apply(index: Int) = {
    @tailrec
    def applyApp(index: Int, xs: ListImpl[A]) : A = if (index == 0) xs.head else applyApp(index-1, xs.tail)
    if (index >= this.length) throw new IndexOutOfBoundsException else applyApp(index, this)
  }

  def filter(f: A => Boolean) = {
    @tailrec
    def filter(from: ListImpl[A], to: ListImpl[A]): ListImpl[A] = if (from.isEmpty) to else {
      if (f(from.head)) filter(from.tail, from.head +: to) else filter(from.tail, to)
    }
    filter(this, Nil).reverse
  }

  def length = {
    @tailrec
    def length(xs: ListImpl[A], app: Int) : Int = {
      if (xs.isEmpty) app else length(xs.tail, 1 + app)
    }
    length(this, 0)
  }

  def reverse: ListImpl[A] = {
    @tailrec
    def move(from: ListImpl[A], to: ListImpl[A]): ListImpl[A] =
      if (from.isEmpty) to else move(from.tail, from.head +: to)
    move(this, Nil)
  }
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    @tailrec
    def foldLeftApp(ll:ListImpl[A], app:B) : B = if (ll.isEmpty) app else foldLeftApp(ll.tail, f(app, ll.head))
    foldLeftApp(this, z)
  }

  def foldRight[B](z: B)(f: (A, B) => B): B =
    reverse.foldLeft(z)((right, left) => f(left, right))

  def forAll(p:(A) => Boolean):Boolean = if (this == Nil) true else if (!p(this.head)) false else this.tail.forAll(p)

  def map[B](f: A=>B):ListImpl[B] = {
    def map(ll:ListImpl[A], app: ListImpl[B]): ListImpl[B] = if (ll.isEmpty) app else map(ll.tail, f(ll.head)+:app)
    map(this, Nil).reverse
  }
}

case object Nil extends ListImpl[Nothing] {
  override def head: Nothing = throw new NoSuchElementException("Empty list")

  override def tail: ListImpl[Nothing] = throw new NoSuchElementException("Empty list")

  override def isEmpty: Boolean = true
}

case class ConsImpl[A](head: A, tail:ListImpl[A]) extends ListImpl[A] {
  override def isEmpty: Boolean = false
}

object ListImpl {
  def apply[A](xs: A*): ListImpl[A] = {
    var newList: ListImpl[A] = Nil
    for (x <- xs) newList =  x +: newList
    newList.reverse
  }

}

