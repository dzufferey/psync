package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

//TODO congruence closure to reduce the number of terms in the instanciation

object CongruenceClosure {

  def apply(f: Formula): Seq[CongruenceClass] = {

    var formulaToNode = Map.empty[Formula, CcNode]

    def getNode(f: Formula): CcNode = {
      if (formulaToNode contains f) { 
        formulaToNode(f)
      } else {
        val n = f match {
          case Application(f, args) =>
            val argsN = args.map(getNode(_))
            new CcSym(f, argsN)
          case v @ Variable(_) => new CcVar(v)
          case l @ Literal(_) => new CcLit(l)
          case other =>
            Logger.logAndThrow("CongruenceClosure", Error, "did not expect: " + other)
        }
        formulaToNode += f -> n
        n
      }
    }

    //create the graph
    val gts = FormulaUtils.collectGroundTerms(f)
    gts.foreach( getNode(_) )

    def processEqs(f: Formula): Unit = f match {
      case And(lst @ _*) =>
        lst.foreach(processEqs(_))
      case Eq(a, b) if formulaToNode.contains(a) && formulaToNode.contains(b) =>
        formulaToNode(a).merge(formulaToNode(b))
      case Binding(ForAll | Exists, _, f) =>
        processEqs(f)
      case other =>
        ()
    }
    processEqs(f)
    
    //extract the CC classes
    val cls = formulaToNode.values.groupBy(_.find)
    cls.map{ case (repr, ms) => new CongruenceClass(repr.formula, ms.map(_.formula).toSet) }.toSeq
  }

}

//a container for CC classes
class CongruenceClass(val repr: Formula, val members: Set[Formula]) {
  def contains(f: Formula) = members(f)
}

//node in the CC graph
abstract class CcNode {
  var parent: Option[CcNode] = None
  var ccParents: Set[CcNode] = Set.empty

  def formula: Formula
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
      val toTest = p1.flatMap( x => p2.map( y => (x,y) ) )
      for ( (x,y) <- toTest if x.find != y.find && x.congruent(y)) x.merge(y)
    }
  }

}

class CcSym(symbol: Symbol, args: List[CcNode]) extends CcNode {
  def formula = symbol(args.map(_.formula):_*) 
  def name = symbol.toString
  def arity = args.length
  def getArgs: List[CcNode] = args
}

class CcVar(v: Variable) extends CcNode {
  def formula = v
  def name = v.toString
  def arity = 0
  def getArgs: List[CcNode] = Nil
}

class CcLit(l: Literal[_]) extends CcNode {
  def formula = l
  def name = l.toString
  def arity = 0
  def getArgs: List[CcNode] = Nil
}
