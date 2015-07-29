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
class TermGenerator(_vars: List[Variable],
                    _expr: Formula) {

  val (vars, expr) = Simplify.deBruijnIndex(ForAll(_vars, _expr)) match {
    case ForAll(vs, f) => vs -> f
    case other => Logger.logAndThrow("TermGenerator", Error, "expect ∀, found: " + other)
  }
  
  override def equals(a: Any): Boolean = {
    if (a.isInstanceOf[TermGenerator]) {
      val tg = a.asInstanceOf[TermGenerator]
      tg.vars == vars && tg.expr == expr
    } else false
  }
  override def hashCode: Int = vars.hashCode + expr.hashCode

  /** returns only the newly generated terms, i.e., the terms not already in gts
   *  TODO this is the (semi) brain-dead version...
   */
  def apply(gts: Set[Formula]): Set[Formula] = {
    val gts2 = gts.toVector.groupBy(_.tpe)
    val candidates = vars.map( v => gts2.getOrElse(v.tpe, Vector.empty) ).toVector
    val tuplified = Misc.cartesianProductIterator(candidates)
    var terms = Set[Formula]()
    while (tuplified.hasNext) {
      val ts = tuplified.next
      val map = vars.view.zip(ts).toMap[Formula,Formula]
      val t = FormulaUtils.map( f => map.getOrElse(f, f), expr)
      if (!gts.contains(t)) {
        terms += t
      }
    }
    terms
  }

}
