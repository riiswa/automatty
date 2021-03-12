package com.automatty.automata

import scala.collection.mutable

package object memory {
  trait MemoryManager[-A, +B]

  /** Use this when the automate don't need extra memory */
  case object EmptyMemoryManager extends MemoryManager[Any, Nothing]

  /**
   *
   * @param initial An initial value to add to the memory
   * @param memoryWrite Function that transform an Alphabet type element to a Memory type element
   * @param memoryCheck Function that check if the last memory state if valid
   * @tparam A Alphabet type
   * @tparam B Memory type
   */
  case class PushdownManager[A, B](
                                    initial: B,
                                    memoryWrite: (A, B) => B,
                                    memoryCheck: B => Boolean
                                  ) extends MemoryManager[A, B] {
    /** Add a initial value to the memory */
    val memory = mutable.Stack[B](initial)
  }
}