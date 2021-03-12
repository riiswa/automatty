package com.automatty.sets

object UniversalSet {
  def apply[A] = new UniversalSet[A]
  class UniversalSet[A] extends Set[A] {
    def iterator: Iterator[A] = Iterator() //can't iterate the infinity
    def contains(elem: A): Boolean = true
    def excl(elem: A) = ExclusiveSet(elem)
    def incl(elem: A) = new UniversalSet[A]
  }
}
