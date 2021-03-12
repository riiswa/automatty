import com.automatty._
import com.automatty.implicits._
import com.automatty.automata._
import com.automatty.automata.states._

import org.junit.Assert.assertEquals
import org.junit.Test


class TD1EX1 {
  trait Alphabet

  case object A extends Alphabet
  case object B extends Alphabet
  
  @Test def _1(): Unit = {
    // The number of A is a multiple of 4
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

    assertEquals(automaton.accepts(Nil), true)
    assertEquals(automaton.accepts(List(B, B, B, B)), true)
    assertEquals(automaton.accepts(List(A, B, A)), false)
    assertEquals(automaton.accepts(List(A, A, A, A)), true)
    assertEquals(automaton.accepts(List(A, B, A, B, A, A)), true)
    assertEquals(automaton.accepts(List(A, B, A, B, A, A, A, B, A, B, A, A)), true)
  }
  

  @Test def _2(): Unit = {
    // An even number of A and an odd number of B
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
    
    assertEquals(automaton.accepts(Nil), false)
    assertEquals(automaton.accepts(List(A, B)), false)
    assertEquals(automaton.accepts(List(B)), true)
    assertEquals(automaton.accepts(List(A, B, A)), true)
    assertEquals(automaton.accepts(List(A, B, A, B)), false)
  }

  @Test def _3(): Unit = {
    // Any symbol A is preceded and followed by at least one symbol B
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

    assertEquals(automaton.accepts(Nil), true)
    assertEquals(automaton.accepts(List(A, B)), false)
    assertEquals(automaton.accepts(List(B)), true)
    assertEquals(automaton.accepts(List(A, B, A)), false)
    assertEquals(automaton.accepts(List(B, A, B, B, A, B)), true)
  }
}
