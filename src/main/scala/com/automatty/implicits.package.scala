package com.automatty

import com.automatty.automata.FiniteAutomaton.Epsilon
import com.automatty.automata._
import com.automatty.automata.states.State

import scala.language.implicitConversions
import com.automatty.sets.SingletonSet

package object implicits {
  
  implicit def any2SingletonOfOpt[A](x: A): Traversable[Option[A]] = SingletonSet(Option(x))
  
  implicit def any2Opt[A](x: A): Option[A] = Some(x)
  
  implicit class TransitionBuilder[A](sal: (State, Set[Option[A]])) {
    def ->[B](s:State): Transition[A, B] = Transition[A, B](sal._1, sal._2, s)
  }
  
  implicit class SetWithEpsilon[A](s: Set[A]) {
    def withEpsilon: Set[Option[A]] = s.map(Option.apply) + Epsilon
  }
                                     
}
