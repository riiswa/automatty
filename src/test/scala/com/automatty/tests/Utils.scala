package com.automatty.tests

private def pathF(td: Int)(ex: Int)(exx: Int, extra: String = ""): String =
  s"src/test/scala/com/automatty/tests/td${td}/ex${ex}/automaton_${td}_${ex}_${exx}${if (!extra.isBlank) s"_$extra" else ""}.png"

trait HavePath(td: Int, ex: Int) {
  def path = pathF(td)(ex)
}
