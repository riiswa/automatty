package com.automatty.automata

import com.automatty.sets._

import guru.nidi.graphviz.attribute.Rank.RankDir
import guru.nidi.graphviz.attribute._
import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.Factory._
import guru.nidi.graphviz.model.Node

import java.io.File

trait RenderableAutomaton[A, B] extends FiniteAutomaton[A, B] {
  def render(path: String): Unit = {
    require(states.size == states.map(_.label).size, "Label of states should be unique for rendering the automata.")

    def acceptedLettersRepr(al: Set[Option[A]]): String = al match {
      case ExclusiveSet(elems:_*)  => s"*\\\\${elems.map {
        case Some(x) => x
        case None => 'ε'
      }.mkString(",")}"
      case UniversalSet => "*"
      case al => al.map {
        case Some(x) => x
        case None => 'ε'
      }.mkString(",")
    }

    def stylishAcceptorState(label: String, n: Node): Node =
      if (acceptorStates.map(_.label).contains(label))
        n.`with`(Shape.DOUBLE_CIRCLE)
        else
        n.`with`(Shape.CIRCLE)

    val g = graph("automaton").directed()
      .graphAttr().`with`(Rank.dir(RankDir.LEFT_TO_RIGHT))

    val nodes: Map[String, (Node, List[(Node, String)])] =
      states.map(s => (s.label, node(s.label) -> List.empty[(Node, String)])).toMap

    val initialNodeMarkers = 
      initialStates.map(s => node("").link(nodes(s.label)._1).`with`(Size.std().size(0, 0)))

    Graphviz.fromGraph(g.`with`(
      (transitions.foldLeft(nodes)(
        (nodes, t) => nodes.updated(t.s1.label, nodes(t.s1.label) match {
          case (n, l) => (n, (nodes(t.s2.label)._1, acceptedLettersRepr(t.acceptedLetters)) :: l)
        })
      ).map {
        case (label, (n, l)) => stylishAcceptorState(
          label,
          n.link(l.map { case (next, repr) => to(next).`with`(Label.of(repr)) }: _*))
      }.toList ++ initialNodeMarkers): _*
    )).render(Format.PNG).toFile(new File(path))
  }
}
