package com.automatty.sets

case class ExclusiveSet[A](elems: A*) extends Set[A] {
  @deprecated("Can't iterate through an ExclusiveSet, use a classic Set instead.")
  def iterator: Iterator[A] = Iterator()
  def contains(elem: A): Boolean = !elems.contains(elem)
  def excl(elem: A): Set[A] = ExclusiveSet((elems :+ elem):_*)
  def incl(elem: A): Set[A] = elems.filter(x => x != elem) match {
    case se if se.isEmpty => UniversalSet[A]
    case se => ExclusiveSet(se:_*)
  }
}
