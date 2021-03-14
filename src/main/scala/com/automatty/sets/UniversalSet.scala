package com.automatty.sets

object UniversalSet {
  def apply[A] = new UniversalSet[A]
  class UniversalSet[A] extends Set[A] {
    
    @deprecated("Can't iterate through an UniversalSet, use a classic Set instead.")
    def iterator: Iterator[A] = Iterator()
    def contains(elem: A): Boolean = true
    def excl(elem: A) = ExclusiveSet(elem)
    def incl(elem: A) = new UniversalSet[A]
  }
}
