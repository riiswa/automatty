package com.automatty.tests.td1.ex2

import com.automatty.automata.FiniteAutomaton
import com.automatty.automata.states._
import com.automatty.implicits._

import com.automatty.tests.HavePath

import org.scalatest._
import flatspec._
import matchers._

enum Binary:
  case Zero extends Binary
  case One extends Binary
  override def toString: String = this match 
    case Zero => "0"
    case One => "1"


class TD1EX2 extends AnyFlatSpec with should.Matchers with HavePath(1, 2) {
  "An automaton" should "determinate if a integer is even (ex 1.2.1)" in {
    val si = new State("-") with InitialState
    val s0 = new State("0") with AcceptorState
    val s1 = new State("1")

    val automaton = FiniteAutomaton.Deterministic[Binary, Nothing](
      si,
      Set(s0, s1),
      Set(
        si--Binary.Zero->s0,
        s0--Binary.Zero->s0,
        s0--Binary.One->s1,
        s1--Binary.One->s1,
        si--Binary.One->s1,
        s1--Binary.Zero->s0
      )
    )

    automaton.accepts(List(Binary.Zero)) should be (true)
    automaton.accepts(List(Binary.One)) should be (false)
    automaton.accepts(List(Binary.One, Binary.Zero, Binary.One)) should be (false)
    automaton.accepts(List(Binary.One, Binary.Zero, Binary.One, Binary.Zero)) should be (true)
    
    automaton.render(path(1, ""))
  }

  "An automaton" should "determinate if a integer is a multiple of 3 (ex 1.2.2)" in {
    val si = new State("-") with InitialState
    val s0 = new State("0") with AcceptorState
    val s1 = new State("1")
    val s2 = new State(label="2")

    val automaton = FiniteAutomaton.Deterministic[Binary, Nothing](
      si,
      Set(s0, s1, s2),
      Set(
        si--Binary.Zero->s0,
        s0--Binary.Zero->s0,
        s0--Binary.One->s1,
        s1--Binary.One->s0,
        si--Binary.One->s1,
        s1--Binary.One->s0,
        s1--Binary.Zero->s2,
        s2--Binary.Zero->s1,
        s2--Binary.One->s2
      )
    )

    automaton.accepts(List(Binary.Zero)) should be (true)
    automaton.accepts(List(Binary.One)) should be (false)
    automaton.accepts(List(Binary.One, Binary.One)) should be (true)
    automaton.accepts(List(Binary.One, Binary.One, Binary.Zero, Binary.Zero)) should be (true)

    automaton.render(path(2, ""))
  }
}
