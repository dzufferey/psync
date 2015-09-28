package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

//process Lt, Leq, Gt, Geq:
//  if type is Int then ✓
//  else
//    replace by a new UnInterpretedFct
//    add EPR axioms for the new functions
//    if type is FSet then ...
//      axioms for point-wise ordering
//      ... !! only a partial order !!
//    else if type is FMap then ...
//      ... ?!?!
//    else if type is Tuple then
//      axioms for lexicographic ordering
//        ...
//    else
//      axioms for total order:
//        ForAll(List(pld1, pld2), And(
//          Or(leq(pld1, pld2), leq(pld2, pld1)),
//          Implies(leq(pld1, pld2) && leq(pld2, pld1), pld1 === pld2)
//        )),
//        ForAll(List(pld1, pld2, pld3),
//          Implies(leq(pld1, pld2) && leq(pld2, pld3), leq(pld1, pld3))
//        ),

object ReduceOrdered {


  def apply(f: Formula): Formula = {
    ???
  }

  def apply(fs: List[Formula]): List[Formula] = fs.map(apply)

}
