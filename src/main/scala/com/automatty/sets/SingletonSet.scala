package com.automatty.sets

case class SingletonSet[A](v: A) extends Set[A] {
  def iterator: Iterator[A] = Iterator(v)
  def contains(elem: A): Boolean = elem == v
  def excl(elem: A): Set[A] = if (contains(elem)) Set.empty[A] else SingletonSet(v)
  def incl(elem: A): Set[A] = if (contains(elem)) Set(v, elem) else SingletonSet(v)
}