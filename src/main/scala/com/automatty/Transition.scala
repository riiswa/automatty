package com.automatty

import scala.collection.mutable

/** Transition between two states
 * 
 * @param s1 First State
 * @param canApply Function to check if the transition can be done
 * @param s2 Second State
 * @tparam A Alphabet type
 * @tparam B Memory type
 */
case class Transition[A, B](
                             s1: State, canApply: A => Boolean, 
                             s2: State
                           ) extends PartialFunction[(State, A, MemoryManager[A, B]), State] {
  /**
   * 
   * @param p
   * @return
   */
  def apply(p: (State, A, MemoryManager[A, B])): State =  {
    p match {
      case (s, a, pm @ PushdownManager(_, memoryWrite, _)) => pm.memory.push(memoryWrite(a, pm.memory.pop()))
      case _ =>
    }
    s2
  }

  /**
   * 
   * @param p
   * @return
   */
  def isDefinedAt(p: (State, A, MemoryManager[A, B])): Boolean = p match {
    case (s, a, pm @ PushdownManager(_, _, memoryCheck)) => s1 == s && canApply(a) && memoryCheck(pm.memory.head)
    case (s, a, EmptyMemoryManager) =>  s1 == s && canApply(a)
  }
  
  /** Transform a --> b transition to b --> a */
  def inverse: Transition[A, B] = Transition(s2, canApply, s2)
}
