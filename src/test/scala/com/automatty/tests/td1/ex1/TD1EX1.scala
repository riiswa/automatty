package com.automatty.tests.td1.ex1

import com.automatty.automata.FiniteAutomaton
import com.automatty.automata.states._
import com.automatty.implicits._

import com.automatty.tests.HavePath

import org.scalatest._
import flatspec._
import matchers._

class TD1EX1 extends AnyFlatSpec with should.Matchers with HavePath(1, 1) {
  "An automaton" should "determinate if the number of A is a multiple of 4 (ex 1.1.1)" in {
    val n0 = new State("0") with InitialState with AcceptorState
    val n1 = new State("1")
    val n2 = new State("2")
    val n3 = new State("3")

    val automaton = FiniteAutomaton.Complete[Alphabet, Nothing](
      n0,
      Set(n1, n2, n3),
      Set(
        n0--A->n1,
        n0--B->n0,
        n1--A->n2,
        n1--B->n1,
        n2--A->n3,
        n2--B->n2,
        n3--A->n0,
        n3--B->n3

      )
    )

    automaton.accepts(Nil) should be (true)
    automaton.accepts(List(B, B, B, B)) should be (true)
    automaton.accepts(List(A, B, A)) should be (false)
    automaton.accepts(List(A, A, A, A)) should be (true)
    automaton.accepts(List(A, B, A, B, A, A)) should be (true)
    automaton.accepts(List(A, B, A, B, A, A, A, B, A, B, A, A)) should be (true)
    
    automaton.render(path(1, ""))
  }
  
  it should "determinate if there is an even number of A and an odd number of B (ex 1.1.2)" in {
    val s00 = new State("(0, 0)") with InitialState
    val s01 = new State("(0, 1)") with AcceptorState
    val s10 = new State("(1, 0)")
    val s11 = new State("(1, 1)")
    val automaton = FiniteAutomaton.Complete[Alphabet, Nothing](
      s00,
      Set(s01, s10, s11),
      Set(
        s00--B->s01,
        s00--A->s10,
        s01--B->s00,
        s01--A->s11,
        s11--B->s10,
        s11--A->s01,
        s10--B->s11,
        s10--A->s00
      )
    )

    automaton.accepts(Nil) should be (false)
    automaton.accepts(List(A, B)) should be (false)
    automaton.accepts(List(B)) should be (true)
    automaton.accepts(List(A, B, A)) should be (true)
    automaton.accepts(List(A, B, A, B)) should be (false)

    automaton.render(path(2, ""))
  }
  
  it should "determinate if Any symbol A is preceded and followed by at least one symbol - Deterministic version (ex 1.1.3)" in {
    val s1 = new State("1") with InitialState with AcceptorState
    val s2 = new State("2") with AcceptorState
    val s3 = new State("3")

    val automaton = FiniteAutomaton.Deterministic[Alphabet, Nothing](
      s1,
      Set(s2, s3),
      Set(
        s1--B->s2,
        s2--B->s2,
        s2--A->s3,
        s3--B->s2
      )
    )

    automaton.accepts(Nil) should be (true)
    automaton.accepts(List(A, B)) should be (false)
    automaton.accepts(List(B)) should be (true)
    automaton.accepts(List(A, B, A)) should be (false)
    automaton.accepts(List(B, A, B, B, A, B)) should be (true)

    automaton.render(path(3, "deterministic"))
  }

  it should "determinate if Any symbol A is preceded and followed by at least one symbol - Complete version (ex 1.1.3)" in {
    val s1 = new State("1") with InitialState with AcceptorState
    val s2 = new State("2") with AcceptorState
    val s3 = new State("3")
    val s4 = new State("4")

    val automaton = FiniteAutomaton.Complete[Alphabet, Nothing](
      s1,
      Set(s2, s3, s4),
      Set(
        s1--B->s2,
        s2--B->s2,
        s2--A->s3,
        s3--B->s2,
        s1--A->s4,
        s4--Set(A, B)->s4,
        s3--A->s4
        
      )
    )

    automaton.accepts(Nil) should be (true)
    automaton.accepts(List(A, B)) should be (false)
    automaton.accepts(List(B)) should be (true)
    automaton.accepts(List(A, B, A)) should be (false)
    automaton.accepts(List(B, A, B, B, A, B)) should be (true)

    automaton.render(path(3, "complete"))
  }
}
