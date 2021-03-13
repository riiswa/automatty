package com.automatty.automata

package object states {

  trait StateAncestor

  case object ErrorState extends StateAncestor

  trait AcceptorState extends StateAncestor

  trait InitialState extends StateAncestor

  class State(val label: String = "") extends StateAncestor {
    override def toString: String = label

    

    //def -->[A, B](state: State)(implicit f: (A => Boolean)) = Transition[A, B](this, f, state)

    def --[A](al: Traversable[Option[A]]): (State, Set[Option[A]])  = (this, al.toSet)
    //def --[A](al: Traversable[A]): (State, Set[Option[A]])  = (this, al.toSet)
    //def --[A](al: Traversable[A]): (State, Set[Option[A]])  = (this, al.map(Option.apply).toSet)
   
  }

  
}
