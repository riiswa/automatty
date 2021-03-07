package com.automatty

import scala.collection.mutable

/** Finite automaton ancestor
 * 
 * @tparam A Alphabet type
 * @tparam B Memory type
 */
sealed trait FiniteAutomaton[A, B] {
  def transitions: Set[Transition[A, B]]

  def mm: MemoryManager[A, B]
  
  def getStates: Set[State]

  /** Check if the automaton accepts a word
   * 
   * @param word The word
   * @return true if the automaton accepts the word, false if not
   */
  def accepts(word: List[A]): Boolean

  /** Check if all the states in transitions are defined in the set of states */
  require(transitions.forall{
    case Transition(s1, _, s2) => Set(s1, s2).subsetOf(getStates)
  }, "Transitions states should be defined in the automaton set of states.")
  
}

/** Finite automaton implementation */
object FiniteAutomaton {

  /** Implementation of an non deterministic automate
   * 
   * @param states Set of the automaton states
   * @param transitions Set of the automaton transition
   * @param mm Memory manager
   * @tparam A Alphabet type
   * @tparam B Memory type
   */
  case class Nondeterministic[A, B](
                                     states: Set[State],
                                     transitions: Set[Transition[A, B]],
                                     mm: MemoryManager[A, B] = EmptyMemoryManager
                                   ) extends FiniteAutomaton[A, B] {
    def getStates = states

    def accepts(word: List[A]): Boolean = {
      @annotation.tailrec
      def execute(word: List[A], currentStates: Set[State]): Set[State] = word match
        case Nil => currentStates
        case h :: t => execute(t, transitions.flatMap(pf => currentStates.collect {
          case s if pf.isDefinedAt((s, h, mm)) => pf((s, h, mm))
        }))

      execute(word, states.filter(s => s.isInstanceOf[InitialState])).exists(_.isInstanceOf[AcceptorState])
    }


  }

  /** Deterministic automaton common method ancestor
   * 
   * @tparam A Alphabet type
   * @tparam B Memory type
   */
  sealed trait DeterministicAncestor[A, B] extends FiniteAutomaton[A, B] {
    def initialState: State with InitialState

    def states: Set[State]

    def getStates = Set(initialState) ++ states

    @annotation.tailrec
    final def execute(word: List[A], currentState: State): StateAncestor = word match
      case Nil => currentState
      case h :: t => transitions.find(pf => pf.isDefinedAt((currentState, h, mm))) match
        case None => ErrorState
        case Some(pf) => execute(t, pf((currentState, h, mm)))
  }

  /** Implementation of a deterministic automate
   * 
   * @param initialState An unique initial state
   * @param states Set of the automaton states
   * @param transitions Set of the automaton transition
   * @param mm Memory Manager
   * @tparam A Alphabet type
   * @tparam B Memory type
   */
  case class Deterministic[A, B](
                                  initialState: State with InitialState,
                                  states: Set[State],
                                  transitions: Set[Transition[A, B]],
                                  mm: MemoryManager[A, B] = EmptyMemoryManager
                                ) extends DeterministicAncestor[A, B] {

    def accepts(word: List[A]): Boolean = execute(word, initialState).isInstanceOf[AcceptorState]

  }

  /** Implementation of a complete automate
   *
   * @param initialState An unique initial state
   * @param states Set of the automaton states
   * @param transitions Set of the automaton transition
   * @param mm Memory Manager
   * @tparam A Alphabet type
   * @tparam B Memory type
   */
  case class Complete[A, B](
                             initialState: State with InitialState,
                             states: Set[State],
                             transitions: Set[Transition[A, B]],
                             mm: MemoryManager[A, B] = EmptyMemoryManager
                           ) extends DeterministicAncestor[A, B] {
    def accepts(word: List[A]): Boolean = execute(word, initialState) match {
      /** The ErrorState throws an Error for an complete Automaton */
      case ErrorState => throw new Exception("The automaton should be complete.")
      case state => state.isInstanceOf[AcceptorState]
    }
  }

}
