package psync.logic

import psync.formula._

import dzufferey.utils.Misc
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

/** define a term generation function (used to generate additional ground terms for ψ-local theory extensions)
 * @param vars are the free variables in expr that are replaced with ground terms during the generation
 * @param expr is the template expression to generate
 * @param modifiers are a list of function that can additionally be used to check candiates variables
 */
class TermGenerator(vars: List[Variable],
                    expr: Formula) {

  /** returns only the newly generated terms, i.e., the terms not already in gts
   *  TODO this is the (semi) brain-dead version...
   */
  def apply(gts: Set[Formula]): Set[Formula] = {
    val gts2 = gts.toVector.groupBy(_.tpe)
    val candidates = vars.toVector.map( v => {
      gts2.toVector.flatMap{ case (tpe, ts) => Typer.unify(v.tpe, tpe) match {
        case Some(map) =>
          ts.toVector.map(map -> _)
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
    val tuplified = Misc.cartesianProductIterator(candidates)
    var terms = Set[Formula]()
    while (tuplified.hasNext) {
      val (subst, ts) = tuplified.next.unzip
      mergeMap(subst) match {
        case Some(s) =>
          val map = vars.view.map(FormulaUtils.copyAndType(s, _)).zip(ts).toMap[Formula,Formula]
          val expr2 = FormulaUtils.copyAndType(s, expr)
          val t = FormulaUtils.map( f => map.getOrElse(f, f), expr2)
          assert(Typer(t).success)
          if (!gts.contains(t)) {
            terms += t
          }
        case None => ()
      }
    }
    terms
  }

}
