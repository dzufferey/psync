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
  def printVisJS(out: BufferedWriter): Unit

  def storeGraphviz(fileName: String) {
    IO.writeInFile(fileName, printGraphviz(_))
  }

  def storeVisJS(fileName: String) {
    IO.writeInFile(fileName, printVisJS(_))
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
      writeln(n.idx + " [label=\"{" + label + "}\"];")
    }
    def edge(e: Edge) {
      val label = e.term
      writeln(e.src + " -> " + e.dst + " [label=\"" + label + "\"];")
    }
    writeln("digraph IQ {")
    writeln("  node [shape=record];")
    nodes.foreach(n => node(n._2))
    edges.foreach(edge)
    writeln("}")
  }

  def printVisJS(out: BufferedWriter) {
    def writeln(str: String) {
      out.write(str); out.newLine
    }
    def node(n: Node) = {
      //val label = (Seq(n.formula) ++ n.newGroundTerms).map(f => xml.Utility.escape(f.toString)).mkString("\\n")
      val label = (Seq(n.formula) ++ n.newGroundTerms).map(_.toString).mkString("\\n")
      "{id: "+n.idx+", label: '"+label+"', shape: 'box'}"
    }
    def edge(e: Edge) = {
      //val label = xml.Utility.escape(e.term.toString)
      val label = e.term.toString
      "{from: "+e.src+", to: "+e.dst+", label: '"+label+"', font: {align: 'horizontal'}}"
    }
    writeln("""<!doctype html>
<html>
<head>
  <meta charset="UTF-8"> 
  <title>Quantifier instantiation graph</title>
      
  <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/vis/4.14.0/vis.min.js"></script>
  <link href="https://cdnjs.cloudflare.com/ajax/libs/vis/4.14.0/vis.min.css" rel="stylesheet" type="text/css" />
          
  <style type="text/css">
    #qigraph {
      width: 90vw;
      height: 90vh;
      border: 1px solid lightgray;
    }
  </style>
</head>
<body>
<div id="qigraph"></div>

<script type="text/javascript">
  // create an array with nodes
  var nodes = new vis.DataSet([""")
    writeln(nodes.map(n => node(n._2)).mkString("    ", ",\n    ",""))
    writeln("""  ]);

  // create an array with edges
  var edges = new vis.DataSet([""")
    writeln(edges.map(edge).mkString("    ", ",\n    ",""))
    writeln("""  ]);

  // create a network
  var container = document.getElementById('qigraph');
  var data = {
    nodes: nodes,
    edges: edges
  };
  var options = {
    layout: {
      hierarchical: {
        direction: "UD",
        sortMethod: "directed",
        levelSeparation: 500,
        nodeSpacing: 400
      }
    },
    interaction: {
        dragNodes :false
    },
    edges: {
      arrows: {to : true }
    },
    physics: {
      enabled: false
    },
    configure: {
      filter: function (option, path) {
        if (path.indexOf('hierarchical') !== -1) {
          return true;
        }
        return false;
      },
      showButton:false
    }
  };
  var network = new vis.Network(container, data, options);
</script>
</body>
</html>
""")
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
  def printVisJS(out: BufferedWriter) { }
}
