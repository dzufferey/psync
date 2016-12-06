package psync.logic

import psync.formula._

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

  def sym(t: Type) = {
    val suffix = Names.tpe(t)
    UnInterpretedFct("_lt_" + suffix, Some(t ~> t ~> Bool), Nil)
  }

  protected def mkAxioms(t: Type) = t match {
    case Int =>
      Nil
    case FSet(_) | FMap(_, _) | Product(_) | FOption(_) | Bool | Wildcard =>
      Logger.logAndThrow("ReduceOrdered", Error, "not yet supported: " + t)
    case Function(_, _) =>
      Logger.logAndThrow("ReduceOrdered", Error, "order on " + t + " is not defined.")
    case t =>
      val lt = sym(t)
      val pld1 = Variable("pld1").setType(t)
      val pld2 = Variable("pld2").setType(t)
      val pld3 = Variable("pld3").setType(t)
      List(
        ForAll(List(pld1, pld2), Or( And(    lt(pld1, pld2) , Not(lt(pld2, pld1)), Not(Eq(pld1, pld2))),
                                     And(Not(lt(pld1, pld2)),     lt(pld2, pld1) , Not(Eq(pld1, pld2))),
                                     And(Not(lt(pld1, pld2)), Not(lt(pld2, pld1)),     Eq(pld1, pld2) ))),
        ForAll(List(pld1, pld2, pld3), Implies(And(lt(pld1, pld2), lt(pld2, pld3)), lt(pld1, pld3)))
      )
  }

  protected def replaceLt(f: Formula): Formula = f match {
    case l @ Lt(a,b) =>
      if (a.tpe == Int) l
      else sym(a.tpe)(a, b)
    case Application(Lt | Leq | Geq | Gt, x :: _) if (x.tpe != Int) =>
      Logger.logAndThrow("ReduceOrdered", Error, "not normalized: " + f)
    case other => other
  }
  
  protected def collectType(f: Formula): Set[Type] = {
    FormulaUtils.collect[Set[Type]](Set.empty, (acc, t) => t match {
      case Application(Lt | Leq | Geq | Gt, x :: _) => acc + x.tpe
      case _ => acc
    }, f)
  }


  def apply(fs: List[Formula]): List[Formula] = {
    val tpes = collectType(And(fs:_*))
    val axioms = tpes.toList.flatMap(mkAxioms)
    val replaced = fs.map(FormulaUtils.map(replaceLt, _))
    axioms ::: replaced
  }

}
