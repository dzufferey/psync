package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

//congruence closure to reduce the number of terms in the instanciation
trait CC {
  def repr(f: Formula): Formula
  def normalize(f: Formula): Formula
  def groundTerms: Set[Formula]
}

class CongruenceClosure extends CC {
    
  import scala.collection.mutable.{ArrayBuffer, Map => MMap}
  protected val formulaToNode = MMap[Formula, CcNode]()
  protected val symbolToNodes = MMap[Symbol, ArrayBuffer[CcNode]]()
    
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
          val existing = symbolToNodes.getOrElseUpdate(f, ArrayBuffer[CcNode]())
          existing.foreach( n => if (node.congruent(n)) node.merge(n) )
          existing.append(node)
          //return the newly inserted node
          node
        case v @ Variable(_) => new CcVar(v)
        case l @ Literal(_) => new CcLit(l)
        case other =>
          Logger.logAndThrow("CongruenceClosure", Error, "did not expect: " + other)
      }
      formulaToNode += f -> n
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
  
  def normalize(f: Formula): Formula = {
    FormulaUtils.stubornMapTopDown(repr(_), f)
  }

  def groundTerms = formulaToNode.keysIterator.toSet

  def cc: CongruenceClasses = {
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
  
  def apply(f: Seq[Formula]) { apply(And(f:_*)) }

  def apply(f: Formula) {
    FormulaUtils.collectGroundTerms(f).foreach( getNode(_) )
    processEqs(f)
  }


}

object CongruenceClosure {

  def apply(f: Seq[Formula]): CongruenceClasses = apply(And(f:_*))

  def apply(f: Formula): CongruenceClasses = {
    val graph = new CongruenceClosure
    graph(f)
    graph.cc
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

  def arity: Int
  def getArgs: List[CcNode]
  def name: String

  def find: CcNode = parent match {
    case Some(p) =>
      val p2 = p.find
      parent = Some(p2)
      p2
    case None =>
      this
  }

  def union(that: CcNode) {
    val n1 = this.find
    val n2 = that.find
    n1.parent = Some(n2)
    n2.ccParents = n1.ccParents ++ n2.ccParents
    n1.ccParents = Set.empty
  }
    
  def ccPar = find.ccParents

  def congruent(that: CcNode) = {
    name == that.name &&
    arity == that.arity &&
    getArgs.zip(that.getArgs).forall{ case (a,b) => a.find == b.find }
  }

  def merge(that: CcNode) {
    if (find != that.find) {
      val p1 = this.ccPar
      val p2 = that.ccPar
      union(that)
      for ( x <- p1; y <- p2 if x.find != y.find && x.congruent(y) ) x.merge(y)
    }
  }

}

class CcSym(f: Formula, symbol: Symbol, args: List[CcNode]) extends CcNode(f) {
  def name = symbol.toString
  def arity = args.length
  def getArgs: List[CcNode] = args
}

class CcVar(v: Variable) extends CcNode(v) {
  def name = v.toString
  def arity = 0
  def getArgs: List[CcNode] = Nil
}

class CcLit(l: Literal[_]) extends CcNode(l) {
  def name = l.toString
  def arity = 0
  def getArgs: List[CcNode] = Nil
}
