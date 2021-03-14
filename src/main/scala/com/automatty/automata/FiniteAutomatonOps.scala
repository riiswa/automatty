package com.automatty.automata

import com.automatty.automata.memory.MemoryManager
import com.automatty.automata.states._

trait FiniteAutomatonOps[A, B] extends FiniteAutomaton[A, B] with RenderableAutomaton[A, B] {
  protected def complementStateMap: Map[State, State] = states.map(s => s -> (s match {
    case _: AcceptorState with InitialState => new State(s.label) with InitialState
    case _: AcceptorState => new State(s.label)
    case s => s
  })).toMap

  protected def statesWithoutInitial: Map[State, State] = states.map(s => s -> (s match {
    case _: AcceptorState with InitialState => new State(s.label) with AcceptorState
    case _: InitialState => new State(s.label)
    case s => s
  })).toMap

  def parent = this

  def mm: MemoryManager[A, B]

  /** Create the complement of the automaton */
  def complement: FiniteAutomatonOps[A, B]

  /** Create an automaton with inverted transitions */
  def inverse: FiniteAutomatonOps[A, B]

  /** Create a transposed automaton, i.e. its complement and then its inverse. */
  def transpose: FiniteAutomatonOps[A, B] = inverse.complement

  def union(fa: FiniteAutomaton[A, B]): FiniteAutomaton.Union[A, B] = 
    FiniteAutomaton.Union(Set(this, fa))

  def intersect(fa: FiniteAutomaton[A, B]): FiniteAutomaton.Intersection[A, B] = 
    FiniteAutomaton.Intersection(Set(this, fa))
}
