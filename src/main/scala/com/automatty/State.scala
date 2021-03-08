package com.automatty

trait StateAncestor

case object ErrorState extends StateAncestor

trait AcceptorState extends StateAncestor

trait InitialState extends StateAncestor

class State(val label: String = "") extends StateAncestor {
  override def toString: String = label
  
  def -->[A, B](state: State)(implicit f: (A => Boolean)) = Transition(this, f, state)

}
