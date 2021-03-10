package com.automatty

trait StateAncestor

case object ErrorState extends StateAncestor

trait AcceptorState extends StateAncestor

trait InitialState extends StateAncestor

class State(val label: String = "") extends StateAncestor {
  override def toString: String = label
  
  def -->[A, B](state: State)(implicit f: (A => Boolean)) = Transition[A, B](this, f, state)
  
  def --[A](f: (A => Boolean)): (State, A => Boolean)  = (this, f)

}

implicit class TransitionBuilder[A](sf: (State, A => Boolean)) {
  def ->[B](s:State): Transition[A, B] = Transition[A, B](sf._1, sf._2, s)
}
