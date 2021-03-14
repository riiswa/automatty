package com.automatty.automata

import com.automatty.automata.FiniteAutomaton.Epsilon
import com.automatty.automata.states._
import com.automatty.automata.memory._

case class Transition[A, B](
                             s1: State, 
                             acceptedLetters: Set[Option[A]], 
                             s2: State
                           ) extends PartialFunction[(State, A, MemoryManager[A, B]), State] {

  def apply(p: (State, A, MemoryManager[A, B])): State = {
    p match {
      case (s, a, pm@PushdownManager(_, memoryWrite, _)) => pm.memory.push(memoryWrite(a, pm.memory.pop()))
      case _ =>
    }
    s2
  }

  def containsEpsilon: Boolean = acceptedLetters.contains(Epsilon)

  def isDefinedAt(p: (State, A, MemoryManager[A, B])): Boolean = p match {
    case (s, a, pm@PushdownManager(_, _, memoryCheck)) => 
      s1 == s && acceptedLetters.contains(Some(a)) && memoryCheck(pm.memory.head)
    case (s, a, EmptyMemoryManager) => s1 == s && acceptedLetters.contains(Some(a))
  }
  
  def inverse: Transition[A, B] = Transition(s2, acceptedLetters, s2)
}
