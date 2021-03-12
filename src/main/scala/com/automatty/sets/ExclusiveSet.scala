package com.automatty.sets

case class ExclusiveSet[A](elems: A*) extends Set[A] {
  def iterator: Iterator[A] = Iterator() //can't iterate the infinity
  def contains(elem: A): Boolean = !elems.contains(elem)
  def excl(elem: A): Set[A] = ExclusiveSet((elems :+ elem):_*)
  def incl(elem: A): Set[A] = elems.filter(x => x != elem) match {
    case se if se.isEmpty => UniversalSet[A]
    case se => ExclusiveSet(se:_*)
  }
}
