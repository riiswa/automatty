# Automatty

A lightweight library that implements automata with the Scala3 compiler (Dotty). 

Related to ENSEIRB-MATMECA module IF114: "Finite automata and applications".

Note: This project is under construction.

## Example:

```scala
import com.automatty._

trait Alphabet
case object A extends Alphabet
case object B extends Alphabet

// Automata that check if any symbol A is preceded and followed by at least one symbol B
val s1 = new State("1") with InitialState with AcceptorState
val s2 = new State("2") with AcceptorState
val s3 = new State("3")

val isA = (a: Alphabet) => a == A
val isB = (b: Alphabet) => b == B

val automaton = FiniteAutomaton.Deterministic[Alphabet, Nothing](
  s1,
  Set(s2, s3),
  Set(
    s1--isB->s2,
    s2--isB->s2,
    s2--isA->s3,
    s3--isB->s2
  )
)

automaton.accepts(automaton.accepts(List(B, A, B, B, A, B)) // returns True
```

## sbt project compiled with Scala 3

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, test it with `sbt test`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).
