package round.logic

import round.formula._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

trait TypeStratification {

  /** When TypeStratification(t1, t2), clause of the form `∀ x:t1. P(f(x):t2)` are directly sent to the SMT solver.
   *  A TypeStratification is a strict partial order: irreflexive and transitive.
   */
  def apply(t1: Type, t2: Type): Boolean

  /** If type t1 is strictly smaller than t2 then a variable of type t2 is allowed to generates terms of type t1. */
  def lt(t1: Type, t2: Type): Boolean = apply(t2, t1)

  /** A formula is stratified if all its quantified variables are. */ 
  def isStratified(f: Formula): Boolean = Quantifiers.isStratified(f, this.lt)

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
