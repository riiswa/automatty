package com.automatty.automata

import com.automatty.automata.states.State

trait FiniteAutomatonSet[A, B] extends Set[FiniteAutomaton[A, B]] with FiniteAutomaton[A, B] {
  def fas: Set[FiniteAutomaton[A, B]]

  def states: Set[State] = fas.flatMap(_.states)

  def transitions: Set[Transition[A, B]] = fas.flatMap(_.transitions)

  def iterator: Iterator[FiniteAutomaton[A, B]] = fas.iterator

  def contains(elem: FiniteAutomaton[A, B]): Boolean = fas.contains(elem)
}
