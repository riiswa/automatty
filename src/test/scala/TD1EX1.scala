import com.automatty._
import org.junit.Assert.assertEquals
import org.junit.Test

trait Alphabet

case object A extends Alphabet
case object B extends Alphabet

class TD1EX1 {
  @Test def _1(): Unit = {
    // The number of A is a multiple of 4
    val n0 = new State("0") with InitialState with AcceptorState
    val n1 = new State("1")
    val n2 = new State("2")
    val n3 = new State("3")

    val isA = (a: Alphabet) => a == A
    val isB = (b: Alphabet) => b == B

    val automaton = FiniteAutomaton.Complete[Alphabet, Nothing](
      n0,
      Set(n1, n2, n3),
      Set(
        (n0 --> n1)(isA),
        (n0 --> n0)(isB),
        (n1 --> n2)(isA),
        (n1 --> n1)(isB),
        (n2 --> n3)(isA),
        (n2 --> n2)(isB),
        (n3 --> n0)(isA),
        (n3 --> n3)(isB)
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
        (s00 --> s01)(x => x == B),
        (s00 --> s10)(x => x == A),
        (s01 --> s00)(x => x == B),
        (s01 --> s11)(x => x == A),
        (s11 --> s10)(x => x == B),
        (s11 --> s01)(x => x == A),
        (s10 --> s11)(x => x == B),
        (s10 --> s00)(x => x == A)
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

    val isA = (a: Alphabet) => a == A
    val isB = (b: Alphabet) => b == B

    val automaton = FiniteAutomaton.Deterministic[Alphabet, Nothing](
      s1,
      Set(s2, s3),
      Set(
        (s1 --> s2)(isB),
        (s2 --> s2)(isB),
        (s2 --> s3)(isA),
        (s3 --> s2)(isB)
      )
    )

    assertEquals(automaton.accepts(Nil), true)
    assertEquals(automaton.accepts(List(A, B)), false)
    assertEquals(automaton.accepts(List(B)), true)
    assertEquals(automaton.accepts(List(A, B, A)), false)
    assertEquals(automaton.accepts(List(B, A, B, B, A, B)), true)
  }
}
