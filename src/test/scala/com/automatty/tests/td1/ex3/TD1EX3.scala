package com.automatty.tests.td1.ex3

import com.automatty.automata.FiniteAutomaton
import com.automatty.automata.FiniteAutomaton.Epsilon
import com.automatty.automata.states._
import com.automatty.implicits._
import com.automatty.sets._

import com.automatty.tests.HavePath

import org.scalatest._
import flatspec._
import matchers._

trait Alphabet

case object A extends Alphabet
case object B extends Alphabet
case object C extends Alphabet

class TD1EX3 extends AnyFlatSpec with should.Matchers with HavePath(1, 3) {
  "An automaton" should "determinate if a word ends with AA (ex 1.3.1)" in {
    val s1 = new State("1") with InitialState
    val s2 = new State("2")
    val s3 = new State("3") with AcceptorState

    val automaton = FiniteAutomaton.Nondeterministic[Alphabet, Nothing](
      Set(s1, s2, s3),
      Set(
        s1--ExclusiveSet(Epsilon)->s1,
        s1--A->s2,
        s2--A->s3
      )
    )

    automaton.accepts(Nil) should be (false)
    automaton.accepts(List(A, A)) should be (true)
    automaton.accepts(List(A, B, A, A)) should be (true)
    automaton.accepts(List(A, B, A)) should be (false)

    automaton.render(path(1, ""))
  }
  
  it should "determinate if the last letter previously appears in the words" in {
    def buildAutomaton(l: Alphabet): FiniteAutomaton.Nondeterministic[Alphabet, Nothing] = {
      val otherLetters = (Set(A, B, C) - l).map(Option.apply)
      val s0 = new State(s"0$l") with InitialState
      val s1 = new State(s"1$l")
      val ok = new State(s"ok$l") with AcceptorState {}
      
      val automaton = FiniteAutomaton.Nondeterministic[Alphabet, Nothing](
        Set(s0, s1, ok),
        Set(
          s0--otherLetters->s0,
          s0--l->s1,
          s1--ExclusiveSet(Epsilon)->s1,
          s1--l->ok
        )
      )

      automaton.render(path(2, l.toString))
      
      return automaton
    }
    
    val automaton = FiniteAutomaton.Union(Set(A, B, C).map(buildAutomaton))

    automaton.accepts(List(A, B, A, B, C)) should be (false)
    automaton.accepts(List(A, B, A, C, A)) should be (true)
  }
}
