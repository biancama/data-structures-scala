package org.biancama.algorithms.stack

/**
  * Created by massimo on 24/09/16.
  */

trait Stack[+A] {
  def top: A ;

  def isEmpty:Boolean;

  def pop: StackImpl[A];

  def size: Int;

  def push[B >: A](elem: B) : StackImpl[B];
}


case class StackImpl[+A] (ll:List[A]) extends Stack[A]{
  def top: A = ll.last;

  def isEmpty:Boolean = ll.isEmpty

  def pop: StackImpl[A] = if (ll.isEmpty) throw new IndexOutOfBoundsException
  else new StackImpl(ll.dropRight(1))

  def size: Int = ll.size

  def push[B >: A](elem: B) : StackImpl[B] = new StackImpl[B](elem +: ll )
}

object Stack {
  def apply[A](ll: List[A]): StackImpl[A] = new StackImpl(ll)

  def apply[A](): StackImpl[A] = new StackImpl(List.empty)
}

