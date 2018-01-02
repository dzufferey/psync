package psync.logic

import psync.formula._

import dzufferey.utils.Misc
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

// sometime, instead of axiomatizing theories, we can rewrite them.
// for instance, `(a,b)._1` can be rewritten to `a` rather than introducing `∀ a, b. (a,b)._1 = a`
// RewriteRule assumes that the formula is in NNF

//TODO add sanity check: test for matching loops

/* A RewriteRule match the free variables in the LHS and replace them in the RHS
 * @param freeVariables a set of free quantified variables
 * @param lhs a formula the term in which variables are matched
 * @param rhs a formula the term in which the expression matching the variables are replaced
 */
case class RewriteRule(freeVariables: Set[Variable],
                       lhs: Formula,
                       rhs: Formula) {

  def lhsSymbols = FormulaUtils.collectSymbols(lhs)
  def rhsSymbols = FormulaUtils.collectSymbols(rhs)

  protected def unify(expr: Formula, pattern: Formula): Option[Map[Variable,Formula]] = (expr, pattern) match {
    case (Application(f1, args1), Application(f2, args2)) if f1 == f2 && args1.length == args2.length =>
      val init: Option[Map[Variable,Formula]] = Some(Map.empty)
      args1.zip(args2).foldLeft(init)( (acc, pair) =>
        acc.flatMap( map1 => Matching.mergeMap(map1, unify(pair._1, pair._2)) )
      )
    case (e, v @ Variable(_)) if freeVariables(v) =>
      Some(Map(v -> e))
    case (a, b) =>
      if (a == b)
        Some(Map.empty[Variable,Formula])
      else
        None
  }

  protected def rewrite(f: Formula): Formula = {
    unify(f, lhs) match {
      case Some(m) =>
        //checks that the types matches
        val init: Option[Map[TypeVariable,Type]] = Some(Map.empty)
        val typeMap = m.foldLeft(init)( (acc, kv) => acc.flatMap( m => {
          val tm = Typer.unify(kv._1.tpe, kv._2.tpe)
          Typer.mergeTypeMap(m, tm)
        }))
        assert(typeMap.isDefined, "RewriteRule or formula is ill-typed ?")
        val rhs2 = FormulaUtils.copyAndType(typeMap.get, rhs)
        //do the substitution 
        def fct(e: Formula) = e match {
          case v @ Variable(_) => m.getOrElse(v, v)
          case e => e
        }
        val res = FormulaUtils.map(fct, rhs2)
        Logger.assert(Typer(res).success, "Rewriting", "not well typed: " + res)
        res
      case None =>
        f
    }
  }

  def apply(f: Formula): Formula = {
    FormulaUtils.stubornMapTopDown(rewrite, f)
  }
  
  def apply(fs: List[Formula]): List[Formula] = fs.map(apply)

}

object Rewriting {

  val rules = {
    val x = Variable("x").setType(TypeVariable("A"))
    val y = Variable("y").setType(TypeVariable("B"))
    val z = Variable("z").setType(TypeVariable("C"))
    val s1 = Variable("s1").setType(FSet(TypeVariable("A")))
    val s2 = Variable("s2").setType(FSet(TypeVariable("A")))
    val m1 = Variable("m1").setType(FMap(TypeVariable("A"),TypeVariable("B")))
    List(
      // pairs
      RewriteRule(Set(x,y),
                  Fst(Tuple(x,y)),
                  x),
      RewriteRule(Set(x,y),
                  Snd(Tuple(x,y)),
                  y),
      // triples
      RewriteRule(Set(x,y,z),
                  Fst(Tuple(x,y,z)),
                  x),
      RewriteRule(Set(x,y,z),
                  Snd(Tuple(x,y,z)),
                  y),
      RewriteRule(Set(x,y,z),
                  Trd(Tuple(x,y,z)),
                  z),
      // options
      RewriteRule(Set(x),
                  Get(FSome(x)),
                  x),
      RewriteRule(Set(x),
                  IsEmpty(x),
                  Not(IsDefined(x))),
      // sets
      RewriteRule(Set(s1,s2),
                  SupersetEq(s1,s2),
                  SubsetEq(s2,s1)),
      RewriteRule(Set(x,s1),
                  Contains(s1,x),
                  In(x,s1)),
      // map
      RewriteRule(Set(x,m1),
                  IsDefinedAt(m1,x),
                  In(x, KeySet(m1))),
      RewriteRule(Set(m1),
                  Size(m1),
                  Cardinality(KeySet(m1)))
      // TODO some more rules, e.g., simplifications can also be expressed as rules
    )
  }

  protected val symbolsToRules: Map[Symbol,List[RewriteRule]] = {
    rules.flatMap( r => r.lhsSymbols.map( s => s -> r) ).groupBy(_._1).map{ case (k,vs) => k -> vs.map(_._2) }
  }

  def apply(f: Formula): Formula = {
    val worklist = scala.collection.mutable.Set[RewriteRule]()
    FormulaUtils.collectSymbols(f).foreach( s => worklist ++= symbolsToRules.getOrElse(s, Nil) )
    var f2 = f
    while (!worklist.isEmpty) {
        val r = worklist.head
        worklist -= r
        //println("applying " + r)
        val f3 = r(f2)
        //println("f3 = " + f3)
        if (f2 != f3) {
          f2 = f3
          r.rhsSymbols.foreach( s => worklist ++= symbolsToRules.getOrElse(s, Nil) )
        }
    }
    f2
  }
  
  def apply(fs: List[Formula]): List[Formula] = fs.map(apply)

}
