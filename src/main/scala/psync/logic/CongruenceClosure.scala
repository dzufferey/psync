package psync.logic

import psync.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._


//congruence closure to reduce the number of terms in the instanciation
trait CC {
  def repr(f: Formula): Formula
  def allRepr: Set[Formula]
  def normalize(f: Formula): Formula
  def groundTerms: Set[Formula]
  def withSymbol(s: Symbol): Seq[Formula]
  def copy: CC
  def mutable: CongruenceClosure
  def immutable: CongruenceClasses
}

class CongruenceClosure extends CC {
    
  import scala.collection.mutable.{ListBuffer, Map => MMap}
  protected var nbrNodes = 0
  protected val formulaToNode = MMap[Formula, CcNode]()
  protected val symbolToNodes = MMap[Symbol, ListBuffer[CcNode]]()
    
  override def toString = immutable.toString //TODO the braindead version

  protected def getNode(f: Formula): CcNode = {
    if (formulaToNode contains f) { 
      formulaToNode(f)
    } else {
      val n = f match {
        case a @ Application(f, args) =>
          val argsN = args.map(getNode(_))
          val node = new CcSym(a, f, argsN)
          for (n <- argsN) n.find.ccParents += node //calling find here for inserting incrementally
          //check according to existing equalities
          val existing = symbolToNodes.getOrElseUpdate(f, ListBuffer[CcNode]())
          existing.foreach( n => if (node.congruent(n)) node.merge(n) )
          existing += node
          //return the newly inserted node
          node
        case v @ Variable(_) => new CcVar(v)
        case l @ Literal(_) => new CcLit(l)
        case other =>
          Logger.logAndThrow("CongruenceClosure", Error, "did not expect: " + other)
      }
      formulaToNode += f -> n
      nbrNodes += 1
      n.seqNbr = nbrNodes
      n
    }
  }
    
  protected def processEqs(f: Formula): Unit = f match {
    case And(lst @ _*) =>
      lst.foreach(processEqs(_))
    case Eq(a, b) if formulaToNode.contains(a) && formulaToNode.contains(b) =>
      formulaToNode(a).merge(formulaToNode(b))
    case Binding(ForAll | Exists, _, f) =>
      processEqs(f)
    case other =>
      ()
  }

  def repr(f: Formula): Formula = {
    if (FormulaUtils.isGround(f)) {
      if (formulaToNode.contains(f)) {
        formulaToNode(f).find.formula
      } else {
        getNode(f).find.formula
      }
    } else {
      f
    }
  }

  def cClass(f: Formula): Seq[Formula] = {
    if (FormulaUtils.isGround(f)) {
      if (formulaToNode.contains(f)) {
        formulaToNode(f).cClass
      } else {
        getNode(f).cClass
      }
    } else {
      Seq(f)
    }
  }
  
  def normalize(f: Formula): Formula = {
    FormulaUtils.stubornMapTopDown(repr(_), f)
  }

  def groundTerms = formulaToNode.keysIterator.toSet

  def withSymbol(s: Symbol) = {
    val builder = new scala.collection.immutable.VectorBuilder[Formula]()
    symbolToNodes.get(s) match {
      case Some(nodes) =>
        nodes.foreach( n => builder += n.formula )
      case None => ()
    }
    builder.result
  }

  def allRepr = formulaToNode.values.map(_.find.formula).toSet

  def mutable = this

  def immutable: CongruenceClasses = {
    val cls = formulaToNode.values.groupBy(_.find)
    val classes = cls.map{ case (_, ms) =>
      import FormulaUtils.FormulaOrdering
      val msf = ms.map(_.formula).toSet
      new CongruenceClass(msf.min, msf) 
    }
    val map = classes.foldLeft(Map.empty[Formula, CongruenceClass])( (acc, c) => {
      c.members.foldLeft(acc)( (a, m) => a + (m -> c) )
    } )
    new CongruenceClasses(classes, map)
  }

  def copy: CongruenceClosure = {
    val cp = new CongruenceClosure
    cp.nbrNodes = nbrNodes
    val nodeMap = MMap[CcNode, CcNode]()
    def copyNode(n: CcNode): CcNode = {
      if (nodeMap contains n) nodeMap(n)
      else {
        val n2 = n.copyButNotVar(copyNode)
        nodeMap += (n -> n2)
        n2
      }
    }

    formulaToNode.foreach{ case (f, n) =>
      cp.formulaToNode += (f -> copyNode(n))
    }
    symbolToNodes.foreach{ case (sym, buffer) =>
     val buffer2 = new ListBuffer[CcNode]()
     buffer.foreach( n => buffer2 += copyNode(n) )
     cp.symbolToNodes += (sym -> buffer2)
    }

    nodeMap.foreach{ case (n1, n2) => n1.copyVarTo(n2, nodeMap) }

    cp
  }
  

  def addConstraints(f: Seq[Formula]) { f.foreach(addConstraints) }

  def addConstraints(f: Formula) {
    FormulaUtils.collectGroundTerms(f).foreach( getNode(_) )
    processEqs(f)
  }


}

object CongruenceClosure {

  def apply(f: Seq[Formula]): CongruenceClasses = apply(And(f:_*))

  def apply(f: Formula): CongruenceClasses = {
    val graph = new CongruenceClosure
    graph.addConstraints(f)
    graph.immutable
  }

}

class CongruenceClasses(cls: Iterable[CongruenceClass], map: Map[Formula, CongruenceClass]) extends CC {

  override def toString = cls.mkString("\n")

  def apply(f: Formula): CongruenceClass = {
    map.getOrElse(f, new CongruenceClass(f, Set(f))) 
  }

  def repr(f: Formula) = {
    if (map.contains(f)) map(f).repr
    else f
  }
  
  lazy val allR = cls.view.map(_.repr).toSet
  def allRepr = allR

  def classes = cls
  
  def knows(f: Formula) = map.contains(f)

  def normalize(f: Formula) = {
    FormulaUtils.stubornMapTopDown(repr(_), f)
  }

  lazy val formula: Formula = {
    if (cls.isEmpty) True()
    else And(cls.toSeq.map(_.formula):_*)
  }

  protected lazy val gts: Set[Formula] = cls.foldLeft(Set[Formula]())( _ ++ _.terms)
  def groundTerms = gts

  protected lazy val s2t = gts.toSeq.collect{ case a @ Application(_, _) => a }.groupBy(_.fct)
  def withSymbol(s: Symbol) = s2t(s)

  def copy = this
  def immutable = this
  def mutable = {
    val c = new CongruenceClosure
    c.addConstraints(formula)
    groundTerms.foreach(c.repr)
    c
  }

}

object CongruenceClasses {
  val empty = new CongruenceClasses(Nil, Map.empty)
}

//a container for CC classes
class CongruenceClass(val repr: Formula, val members: Set[Formula]) {
  override def toString = repr + " <- " + (members - repr).mkString(", ")
  def contains(f: Formula) = members(f)
  def formula: Formula = {
    val eqs = (members - repr).map( Eq(repr, _) ).toSeq
    if (eqs.isEmpty) True()
    else And(eqs:_*)
  }
  def terms: Set[Formula] = Set(repr) ++ members
  def tpe = repr.tpe
  override def equals(a: Any): Boolean = {
    if (a.isInstanceOf[CongruenceClass]) {
      a.asInstanceOf[CongruenceClass].members == members
    } else false
  }
  override def hashCode: Int = members.hashCode
}

//node in the CC graph
abstract class CcNode(val formula: Formula) {
  var parent: Option[CcNode] = None
  var ccParents: Set[CcNode] = Set.empty
  var children: Seq[CcNode] = Seq.empty

  //the seqNbr is used to try to keep the representative stable.
  //older (low seqNbr) are prefered as representative
  var seqNbr = 0

  def arity: Int
  def getArgs: List[CcNode]
  def name: String

  private val hc = formula.hashCode //do we gain from caching the hash ?
  override def hashCode: Int = hc

  def find: CcNode = parent match {
    case Some(p) =>
      val p2 = p.find
      parent = Some(p2)
      p2
    case None =>
      this
  }

  def cClassN: Seq[CcNode] = {
    val n = find
    val buffer = scala.collection.mutable.ListBuffer[CcNode]()
    buffer += n
    def getChildren(n: CcNode) {
      buffer ++= n.children
      n.children.foreach(getChildren)
    }
    getChildren(n)
    buffer.result
  }
  
  def cClass: Seq[Formula] = {
    val n = find
    val buffer = scala.collection.mutable.ListBuffer[Formula]()
    def getChildren(n: CcNode) {
      buffer += n.formula
      n.children.foreach(getChildren)
    }
    getChildren(n)
    buffer.result
  }


  protected def union(that: CcNode) {
    var n1 = this.find
    var n2 = that.find
    if (n1.seqNbr < n2.seqNbr) {
      val tmp = n2
      n2 = n1
      n1 = tmp
    }
    n1.parent = Some(n2)
    n2.ccParents ++= n1.ccParents
    n1.ccParents   = Set.empty
    n2.children ++= n1.children
    n2.children +:= n1
    n1.children   = Seq.empty
  }
    
  def ccPar = find.ccParents

  //not pretty no memory allocation at all
  @inline private def argsCongruent(_a: List[CcNode], _b: List[CcNode]): Boolean = {
    var a = _a
    var b = _b
    while (true) {
      a match {
        case x::xs =>
          b match {
            case y::ys =>
              if (x.find == y.find) {
                a = xs
                b = ys
              } else {
                return false
              }
            case _ =>   return false
          }
        case Nil => 
          b match {
            case Nil => return true
            case _ =>   return false
          }
      }
    }
    sys.error("unreachable")
  }

  //the interalized name + arity for faster comparison
  private val id = CcNode.internalize(name, arity)
  final def congruent(that: CcNode) = {
    id == that.id &&
    argsCongruent(getArgs, that.getArgs)
  }

  def merge(that: CcNode) {
    if (find != that.find) {
      val p1 = this.ccPar
      val p2 = that.ccPar
      union(that)
      for ( x <- p1; y <- p2 if x.find != y.find && x.congruent(y) ) x.merge(y)
    }
  }

  def copyButNotVar(copyFct: CcNode => CcNode): CcNode
  
  def copyVarTo(c: CcNode, copyFct: CcNode => CcNode) {
    c.parent = parent.map(copyFct)
    c.ccParents = ccParents.map(copyFct)
    c.children = children.map(copyFct)
    c.seqNbr = seqNbr
  }

}

class CcSym(f: Formula, symbol: Symbol, args: List[CcNode]) extends CcNode(f) {
  def name = symbol.toString
  def arity = args.length
  def getArgs: List[CcNode] = args
  def copyButNotVar(copyFct: CcNode => CcNode): CcNode = {
    new CcSym(f, symbol, args.map(copyFct))
  }
}

class CcVar(v: Variable) extends CcNode(v) {
  def name = v.toString
  def arity = 0
  def getArgs: List[CcNode] = Nil
  def copyButNotVar(copyFct: CcNode => CcNode): CcNode = new CcVar(v)
}

class CcLit(l: Literal[_]) extends CcNode(l) {
  def name = l.toString
  def arity = 0
  def getArgs: List[CcNode] = Nil
  def copyButNotVar(copyFct: CcNode => CcNode): CcNode = new CcLit(l)
}

object CcNode {

  private var counter = 0l
  private def getNewId = { counter += 1; counter }
  private val known = scala.collection.mutable.Map[String,Long]()
  def internalize(name: String, arity: Int): Long = {
    assert(arity >= 0 && arity < 256)
    val n = known.getOrElseUpdate(name, getNewId)
    (n << 8) | arity
  }
  //only call that method if there is no CongruenceClasses around
  def clear = {
    counter = 0
    known.clear
  }

}
