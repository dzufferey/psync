package round.logic

import round.formula._

import dzufferey.utils.Misc
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

//facility to generate additional ground terms (ψ-local theory extensions)

/** define a term generation function
 * @param vars are the free variables in expr that are replaced with ground terms during the generation
 * @param expr is the template expression to generate
 * @param modifiers are a list of function that can additionally be used to check candiates variables
 */
class TermGenerator(vars: List[Variable],
                    expr: Formula) {

//val (vars, expr) = Simplify.deBruijnIndex(ForAll(_vars, _expr)) match {
//  case ForAll(vs, f) => vs -> f
//  case other => Logger.logAndThrow("TermGenerator", Error, "expect ∀, found: " + other)
//}
//
//override def equals(a: Any): Boolean = {
//  if (a.isInstanceOf[TermGenerator]) {
//    val tg = a.asInstanceOf[TermGenerator]
//    tg.vars == vars && tg.expr == expr
//  } else false
//}
//override def hashCode: Int = vars.hashCode + expr.hashCode

  /** returns only the newly generated terms, i.e., the terms not already in gts
   *  TODO this is the (semi) brain-dead version...
   */
  def apply(gts: Set[Formula]): Set[Formula] = {
    val gts2 = gts.toVector.groupBy(_.tpe)
    val candidates = vars.toVector.map( v => {
      gts2.toVector.flatMap{ case (tpe, ts) => Typer.unify(v.tpe, tpe) match {
        case Some(map) => ts.toVector.map(map -> _)
        case None => Vector.empty
      }}
    })
    def mergeMap(maps: Iterable[Map[TypeVariable, Type]]): Option[Map[TypeVariable, Type]] = {
      def agree(m1: Map[TypeVariable, Type], m2: Map[TypeVariable, Type]) = {
        m1.forall{ case (k,v) => m2.getOrElse(k, v) == v}
      }
      val init: Option[Map[TypeVariable, Type]] = Some(Map.empty)
      maps.foldLeft(init)( (acc, m2) => {
        acc.flatMap( m1 => {
          if (agree(m1, m2) && agree(m2, m1)) {
            Some(m2.foldLeft(m2)( (acc, kv) =>
              if (acc contains kv._1) acc else acc + kv
            ))
          } else {
            None
          }
        })
      })
    }
    def copyAndType(m: Map[TypeVariable, Type], f: Formula): Formula = f match {
      case Literal(l) => Literal(l).setType(f.tpe.alpha(m))
      case Variable(v) => Variable(v).setType(f.tpe.alpha(m))
      case Application(fct, args) =>
        val args2 = args.map(copyAndType(m, _))
        Application(fct, args).setType(f.tpe.alpha(m))
      case Binding(bt, vars, expr) =>
        val vars2 = vars.map(copyAndType(m, _)).asInstanceOf[List[Variable]]
        val expr2 = copyAndType(m, expr)
        Binding(bt, vars2, expr2).setType(f.tpe.alpha(m))
    }
    val tuplified = Misc.cartesianProductIterator(candidates)
    var terms = Set[Formula]()
    while (tuplified.hasNext) {
      val (subst, ts) = tuplified.next.unzip
      mergeMap(subst) match {
        case Some(s) =>
          val map = vars.view.map(copyAndType(s, _)).zip(ts).toMap[Formula,Formula]
          val expr2 = copyAndType(s, expr)
          val t = FormulaUtils.map( f => map.getOrElse(f, f), expr2)
          if (!gts.contains(t)) {
            terms += t
          }
        case None => ()
      }
    }
    terms
  }

}
