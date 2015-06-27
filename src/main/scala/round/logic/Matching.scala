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

  /** Finds possible instantiations of the freeVariables in term such that the resulting term is in the ground terms of the cClasses.
   * @param term the term in which variables are matched
   * @param freeVariables the set of variables that needs to be instantiated
   * @return a set of possible instantiation for the freeVariables
   */
  def find(term: Formula, freeVariables: Set[Variable]): Set[Map[Variable,Formula]] = {
    assert(freeVariables.subsetOf(term.freeVariables))
    val emp: Option[Map[Variable,Formula]] = Some(Map())
    def unify(t1: Formula, t2: Formula): Option[Map[Variable,Formula]] = (t1,t2) match {
      case (Application(f1, args1), Application(f2, args2)) if f1 == f2 && args1.length == args2.length =>
        args1.zip(args2).foldLeft(emp)( (acc, t12) => {
          acc.flatMap( m1 => {
            m1.foldLeft(unify(t12._1, t12._2))( (acc, kv) => {
              acc.flatMap( m => if (m.getOrElse(kv._1, kv._2) == kv._2) Some(m + kv) else None )
            })
          })
        })
      case (v @ Variable(_), t2) if freeVariables(v) => Some(Map(v -> t2))
      case (a, b) if a == b => emp
      case _ => None
    }
    term match {
      case Application(f, _) => symbolsToTerms(f).flatMap( gt => unify(term, gt) ).toSet
      case v @ Variable(_) => cClasses.classes.filter(_.tpe == v.tpe).map( c => Map(v -> c.repr) ).toSet
      case _ => Set()
    }
  }

  /** Finds possible instantiations of the freeVariables in term such that the resulting term is in the ground terms of the cClasses.
   *  This methods takes equivalence classes into account: if f(x) = y and g(y) is a ground term them g(f(u)) will match u with x.
   * @param term the term in which variables are matched
   * @param freeVariables the set of variables that needs to be instantiated
   * @return a set of possible instantiation for the freeVariables
   */
  def apply(term: Formula, freeVariables: Set[Variable]): Set[Map[Variable,CongruenceClass]] = {
    assert(freeVariables.subsetOf(term.freeVariables))
    val emp: Set[Map[Variable,CongruenceClass]] = Set(Map())
    def mergeMap(m1: Map[Variable,CongruenceClass], m2: Map[Variable,CongruenceClass]): Option[Map[Variable,CongruenceClass]] = {
      val init: Option[Map[Variable,CongruenceClass]] = Some(m2)
      m1.foldLeft(init)( (acc, kv) => {
        acc.flatMap( m => if (m.getOrElse(kv._1, kv._2) == kv._2) Some(m + kv) else None )
      })
    }
    def merge(sm1: Set[Map[Variable,CongruenceClass]], sm2: => Set[Map[Variable,CongruenceClass]]): Set[Map[Variable,CongruenceClass]] = {
      if (sm1.isEmpty) Set.empty
      else for (m1 <- sm1; m2 <- sm2; m <- mergeMap(m1, m2)) yield m
    }
    def unify(t1: Formula, t2: Formula): Set[Map[Variable,CongruenceClass]] = (t1,t2) match {
      case (Application(f1, args1), Application(f2, args2)) if f1 == f2 && args1.length == args2.length =>
        args1.zip(args2).foldLeft(emp)( (acc, t12) => merge(acc, unifyClass(t12._1, t12._2)) )
      case (v @ Variable(_), t2) if freeVariables(v) => Set(Map(v -> cClasses(t2)))
      case (a, b) if a == b => emp
      case _ => Set.empty
    }
    def unifyClass(t1: Formula, t2: Formula): Set[Map[Variable,CongruenceClass]] = {
      cClasses(t2).members.flatMap( t3 => unify(t1, t3) )
    }
    cClasses.classes.flatMap( c => unifyClass(term, c.repr) ).toSet
  }

}
