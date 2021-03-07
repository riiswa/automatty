package com.automatty

trait StateAncestor

case object ErrorState extends StateAncestor

trait AcceptorState extends StateAncestor

trait InitialState extends StateAncestor

class State(name: String) extends StateAncestor {
  override def toString: String = name
  
  def -->[A, B](state: State)(implicit f: (A => Boolean)) = Transition(this, f, state)
}
