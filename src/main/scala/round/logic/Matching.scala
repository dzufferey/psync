package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

//matching for local quantifier instantiation and term generation

class Matching(cClasses: CongruenceClasses) {

  protected val gts = cClasses.groundTerms

  protected val symbolsToTerms: Map[Symbol,Iterable[Application]] = {
    val apps: Set[Application] = gts.collect{ case a @ Application(sym, _) => a }
    apps.groupBy(_.fct)
  }

//protected val parents: Map[(CongruenceClass,Symbol,Int),Set[CongruenceClass]] = {
//  val emp = Map[(CongruenceClass,Symbol,Int),Set[CongruenceClass]]()
//  gts.foldLeft(emp)( (acc, t) => t match {
//    case app @ Application(s, args) =>
//      args.zipWithIndex.foldLeft(acc)( (acc, ai) => {
//        val (arg, i) = ai
//        val idx = if (FormulaUtils.commutative(s)) 0 else i
//        val key = (cClasses(arg),s,idx)
//        val old = acc.getOrElse(key, Set.empty)
//        val newer = old + cClasses(app)
//        acc + (key -> newer)
//      })
//    case _ => acc
//  })
//}

//def getParent(c: CongruenceClass, s: Symbol, pos: Int): Iterable[CongruenceClass] = {
//  val p = if (FormulaUtils.commutative(s)) 0 else pos
//  parents.getOrElse((c,s,p),Nil)
//}

  def apply(term: Formula, freeVariables: Set[Variable]): Set[Map[Variable,CongruenceClass]] = {
    assert(freeVariables.subsetOf(term.freeVariables))
    val emp: Option[Map[Variable,CongruenceClass]] = Some(Map())
    def unify(t1: Formula, t2: Formula): Option[Map[Variable,CongruenceClass]] = (t1,t2) match {
      case (Application(f1, args1), Application(f2, args2)) if f1 == f2 && args1.length == args2.length =>
        args1.zip(args2).foldLeft(emp)( (acc, t12) => {
          acc.flatMap( m1 => {
            m1.foldLeft(unify(t12._1, t12._2))( (acc, kv) => {
              acc.flatMap( m => if (m.getOrElse(kv._1, kv._2) == kv._2) Some(m + kv) else None )
            })
          })
        })
      case (v @ Variable(_), t2) if freeVariables(v) =>
        Some(Map(v -> cClasses(t2)))
      case (a, b) if a == b =>
        emp
      case _ =>
        None
    }
    term match {
      case Application(f, _) => symbolsToTerms(f).flatMap( gt => unify(term, gt) ).toSet
      case v @ Variable(_) => cClasses.classes.filter(_.tpe == v.tpe).map( c => Map(v -> c) ).toSet
      case _ => Set()
    }
  }

}
