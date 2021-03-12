package com.automatty.automata

import com.automatty.automata.states._
import com.automatty.automata.memory._

def Epsilon = None

sealed trait FiniteAutomaton[A, B] {
  def transitions: Set[Transition[A, B]]

  def mm: MemoryManager[A, B]

  def states: Set[State]

  def accepts(word: Iterable[A]): Boolean

  protected def complementStateMap: Map[State, State] = states.map(s => s -> (s match {
    case _: AcceptorState with InitialState => new State(s.label) with InitialState
    case _: AcceptorState => new State(s.label)
    case s => s
  })).toMap

  
  /** Create the complement of the automaton */
  def complement: FiniteAutomaton[A, B]

  /** Create an automaton with inverted transitions */
  def inverse: FiniteAutomaton[A, B]

  /** Create a transposed automaton, i.e. its complement and then its inverse. */
  def transpose: FiniteAutomaton[A, B] = inverse.complement

  /** Check if all the states in transitions are defined in the set of states */
  require(
    transitions.forall(t => Set(t.s1, t.s2).subsetOf(states)),
    "Transitions states should be defined in the automaton set of states."
  )
}

sealed trait NondeterministicAncestor[A, B] extends FiniteAutomaton[A, B] {
  @annotation.tailrec
  final def execute(currentStates: List[(State, Iterable[A])]): Set[State] =
    if (currentStates.exists { case (_, word) => word.nonEmpty })
      execute(currentStates.flatMap {
        case (state, word) if word.nonEmpty => transitions.collect {
          case pf if pf.isDefinedAt((state, word.head, mm)) =>
            (pf((state, word.head, mm)), word.tail) :: (
              if (pf.containsEpsilon) List((pf.s2, word))
              else Nil
              )
          case pf if pf.containsEpsilon => List((pf.s2, word))
        }.flatten
        case sw => sw :: Nil
      })
      else currentStates.map(_._1).toSet
}

sealed trait DeterministicAncestor[A, B] extends FiniteAutomaton[A, B] {
  def initialState: State with InitialState

  def otherStates: Set[State]

  def states = Set(initialState) ++ otherStates

  @annotation.tailrec
  final def execute(word: List[A], currentState: State): StateAncestor = word match
    case Nil => currentState
    case h :: t => transitions.find(pf => pf.isDefinedAt((currentState, h, mm))) match
      case None => ErrorState
      case Some(pf) => execute(t, pf((currentState, h, mm)))
}

object FiniteAutomaton {

  case class Nondeterministic[A, B](
                                     states: Set[State],
                                     transitions: Set[Transition[A, B]],
                                     mm: MemoryManager[A, B] = EmptyMemoryManager
                                   ) extends NondeterministicAncestor[A, B] {
    def accepts(word: Iterable[A]): Boolean = {
      execute(states.collect { case s if s.isInstanceOf[InitialState] => (s, word) }.toList)
        .exists(_.isInstanceOf[AcceptorState])
    }

    def complement: Nondeterministic[A, B] = {
      val cm = complementStateMap
      Nondeterministic[A, B](states.map(cm), transitions.map{
        case Transition(s1, ca, s2) => Transition(cm(s1), ca, cm(s2))
      }, mm)
    }

    def inverse: Nondeterministic[A, B] = Nondeterministic(states, transitions.map(_.inverse), mm)

  }

  case class Deterministic[A, B](
                                  initialState: State with InitialState,
                                  otherStates: Set[State],
                                  transitions: Set[Transition[A, B]],
                                  mm: MemoryManager[A, B] = EmptyMemoryManager
                                ) extends DeterministicAncestor[A, B] {
    def accepts(word: Iterable[A]): Boolean = execute(word.toList, initialState).isInstanceOf[AcceptorState]

    def complement: Deterministic[A, B] = {
      val cm = complementStateMap
      Deterministic[A, B](cm(initialState).asInstanceOf[State with InitialState], states.map(cm), transitions.map{
        case Transition(s1, ca, s2) => Transition(cm(s1), ca, cm(s2))
      }, mm)
    }

    def inverse: Deterministic[A, B] = Deterministic(initialState, states, transitions.map(_.inverse), mm)

  }

  case class Complete[A, B](
                                  initialState: State with InitialState,
                                  otherStates: Set[State],
                                  transitions: Set[Transition[A, B]],
                                  mm: MemoryManager[A, B] = EmptyMemoryManager
                                ) extends DeterministicAncestor[A, B] {
    def accepts(word: Iterable[A]): Boolean = execute(word.toList, initialState) match {
      /** The ErrorState throws an Error for an complete Automaton */
      case ErrorState => throw new Exception("The automaton should be complete.")
      case state => state.isInstanceOf[AcceptorState]
    }

    def complement: Complete[A, B] = {
      val cm = complementStateMap
      Complete[A, B](cm(initialState).asInstanceOf[State with InitialState], states.map(cm), transitions.map{
        case Transition(s1, ca, s2) => Transition(cm(s1), ca, cm(s2))
      }, mm)
    }

    def inverse: Complete[A, B] = Complete(initialState, states, transitions.map(_.inverse), mm)

  }

}