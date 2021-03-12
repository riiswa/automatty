package com.automatty

import com.automatty.automata._
import com.automatty.automata.states.State

import scala.language.implicitConversions
import com.automatty.sets.SingletonSet

package object implicits {
  
  //implicit def any2SingletonSet[A](elem: A): Set[A | Epsilon.type]= SingletonSet(elem)
  
  //implicit def set2SetWithEpsilon[A](se: Set[A]): AcceptedLetters[A] = se.asInstanceOf[AcceptedLetters[A]]
  
  implicit def any2SingletonOfOpt[A](x: A): Set[Option[A]] = SingletonSet(Option(x))
  implicit def set2SetOfOpt[A](s: Set[A]): Set[Option[A]] = s.map(Option.apply)
  //implicit def any2SingletonSet[A](elem: A): Set[A]= SingletonSet(elem)
  
  implicit class TransitionBuilder[A](sal: (State, Set[Option[A]])) {
    def ->[B](s:State): Transition[A, B] = Transition[A, B](sal._1, sal._2, s)
  }
                                     
}
