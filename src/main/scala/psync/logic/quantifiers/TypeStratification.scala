package psync.logic.quantifiers

import psync.logic.CL
import psync.formula._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

trait TypeStratification {

  /** When TypeStratification(t1, t2), clause of the form `∀ x:t1. P(f(x):t2)` are directly sent to the SMT solver.
   *  A TypeStratification is a strict partial order: irreflexive and transitive.
   */
  def apply(t1: Type, t2: Type): Boolean

  /** If type t1 is strictly smaller than t2 then a variable of type t2 is allowed to generates terms of type t1. */
  def lt(t1: Type, t2: Type): Boolean = apply(t2, t1)

  /** Checks is a variable is stratified is the given formula.
   *  Assumes f does not contains quantifiers
   */
  def isStratified(v: Variable, f: Formula): Boolean = {
    isStratified(ForAll(List(v), f))
  }

  /** A formula is stratified if all its quantified variables are. */ 
  def isStratified(axiom: Formula): Boolean = {
    def isGround(vs: Set[Variable], f: Formula) = f.freeVariables.intersect(vs).isEmpty
    def check(acc: Boolean, vs: Set[Variable], f: Formula) = f match {
      case Application(_, args) =>
        acc && (f.tpe == Bool || args.forall( a => isGround(vs, a) || lt(f.tpe, a.tpe) ) )
      case _ =>
        acc
    }
    val sk = skolemize(axiom)
    FormulaUtils.collectWithScope(true, check, sk)
  }
}

/** Default TypeStratification */
object TypeStratification extends TypeStratification {

  def apply(t1: Type, t2: Type): Boolean = (t1,t2) match {
    //TODO FMap(key: Type, value: Type)
    case (_,FSet(_)) => false //nothing can generate a set
    case (Bool,_) | (_,Bool) => true
    case (Product(lst), t) => t != CL.procType && lst.contains(t)
    case (FSet(t), t2) => t2 == Int || (t2 != CL.procType && t == t2)
    case (FOption(t), t2) => t2 == Int || (t2 != CL.procType && t == t2)
    case (CL.procType, Int | FOption(_)) => true
    case (CL.procType, t2 @ UnInterpreted(_)) => t2 != CL.procType
    case (UnInterpreted(_), Int) => true
    case _ => false
  }

}
