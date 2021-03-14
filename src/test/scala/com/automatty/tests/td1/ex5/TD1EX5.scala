package com.automatty.tests.td1.ex5

import com.automatty.automata.FiniteAutomaton
import com.automatty.automata.states._
import com.automatty.implicits._

import com.automatty.tests.HavePath

import org.scalatest._
import flatspec._
import matchers._

sealed trait Alphabet

case object A extends Alphabet
case object B extends Alphabet

class TD1EX5 extends AnyFlatSpec with should.Matchers with HavePath(1, 5) {
  "An automaton" should "have one initial state after unifying initial states" in {
    val i1 = new State("i1") with InitialState
    val i2 = new State("i2") with InitialState
    val sx = new State("sx") with AcceptorState
    
    val automaton = FiniteAutomaton.Nondeterministic[Alphabet, Nothing](
      Set(i1, i2, sx),
      Set(
        i1--A->sx,
        i2--B->sx
      )
    )

    automaton.initialStates should have size 2
    
    automaton.render(path(0, "before"))
    
    val newAutomaton = automaton.unifyingInitialStates("i")
    
    newAutomaton.initialStates should have size 1

    newAutomaton.render(path(0, "after"))
  }
}
