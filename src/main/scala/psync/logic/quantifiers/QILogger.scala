package psync.logic.quantifiers

import psync.formula._
import psync.logic._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer
import dzufferey.utils.IO

import scala.collection.mutable.{Map => MMap, Set => MSet}

import java.io.BufferedWriter


//Quantifier Instantiation Logger to help understand where/how the current heuristics blow up

//TODO add an "empty logger" when we don't track anything

object QILogger {
  case class Node(idx: Int, formula: Formula, newGroundTerms: Iterable[Formula])
  case class Edge(src: Int, dst: Int, /*variable: Variable,*/ term: Formula)
}

trait QILogger {
  
  import QILogger._
  
  def nodesIterator: Iterable[Node]
  def edgesIterator: Iterable[Edge]

  def reset: Unit
  def addNode(n: Node): Unit
  def addNode(idx: Int, formula: Formula, newGroundTerms: => Iterable[Formula]): Unit
  def addEdge(e: Edge): Unit
  def addEdge(src: Int, dst: Int, /*variable: Variable,*/ term: Formula): Unit

  def printGraphviz(out: BufferedWriter): Unit

  def storeGraphviz(fileName: String) {
    IO.writeInFile(fileName, printGraphviz(_))
  }

}

class BasicQILogger extends QILogger {

  import QILogger._

  protected val nodes = MMap[Int, Node]()
  protected val edges = MSet[Edge]()

  def nodesIterator: Iterable[Node] = nodes.values
  def edgesIterator: Iterable[Edge] = edges

  def reset {
    nodes.clear
    edges.clear
  }

  def addNode(n:Node) {
    Logger.assert(!nodes.contains(n.idx), "QILogger", "index " + n.idx + " already exists.")
    nodes += n.idx -> n
  }

  def addNode(idx: Int, formula: Formula, newGroundTerms: => Iterable[Formula]) {
    addNode(Node(idx, formula, newGroundTerms))
  }
  
  def addEdge(e: Edge) {
    Logger.assert(nodes.contains(e.src), "QILogger", "source " + e.src + " does not exist.")
    Logger.assert(nodes.contains(e.dst), "QILogger", "destination " + e.dst + " does not exist.")
    edges += e
  }

  def addEdge(src: Int, dst: Int, /*variable: Variable,*/ term: Formula) {
    addEdge(Edge(src, dst, /*variable,*/ term))
  }

  def printGraphviz(out: BufferedWriter) {
    def writeln(str: String) {
      out.write(str); out.newLine
    }
    def node(n: Node) {
      val label = n.formula + "|" + n.newGroundTerms.map(_.toString).mkString(", ")
      writeln(n.idx + " [label=\"" + label + "\"];")
    }
    def edge(e: Edge) {
      //val label = e.variable + " <- " + e.term
      val label = e.term
      writeln(e.src + " -> " + e.dst + " [label=\"" + label + "\"];")
    }
    writeln("digraph IQ {")
    nodes.foreach(n => node(n._2))
    edges.foreach(edge)
    writeln("}")
  }

}

class EmptyQILogger extends QILogger {
  import QILogger._
  def nodesIterator: Iterable[Node] = Nil
  def edgesIterator: Iterable[Edge] = Nil
  def reset { }
  def addNode(n: Node) { }
  def addNode(idx: Int, formula: Formula, newGroundTerms: => Iterable[Formula]) { }
  def addEdge(e: Edge) { }
  def addEdge(src: Int, dst: Int, /*variable: Variable,*/ term: Formula) { }
  def printGraphviz(out: BufferedWriter) { }
}
