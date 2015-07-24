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
                    _expr: Formula,
                    val modifiers: (Seq[Formula] => Boolean)*) {

  val (vars, expr) = Simplify.deBruijnIndex(ForAll(_vars, _expr)) match {
    case ForAll(vs, f) => vs -> f
    case other => Logger.logAndThrow("TermGenerator", Error, "expect ∀, found: " + other)
  }
  
  override def equals(a: Any): Boolean = {
    if (a.isInstanceOf[TermGenerator]) {
      val tg = a.asInstanceOf[TermGenerator]
      tg.vars == vars && tg.expr == expr &&
      tg.modifiers.isEmpty && modifiers.isEmpty
    } else false
  }
  override def hashCode: Int = vars.hashCode + expr.hashCode + modifiers.length


  val symbols = FormulaUtils.collectSymbols(expr)

  //TODO a more efficient version that can be used in InstGen
  //normalizing, chaining, etc
  //watchlist, avoiding duplication, etc.

  /** returns only the newly generated terms, i.e., the terms not already in gts
   *  TODO this is the (semi) brain-dead version...
   */
  def apply(gts: Set[Formula]): Set[Formula] = {
    val gts2 = gts.view.filter(t => !FormulaUtils.exists({
        case Application(s, _) => symbols(s)
        case _ => false }, t) ).toVector
    val candidates = vars.map( v => gts2.filter( t => t.tpe == v.tpe )).toVector
    val tuplified = Misc.cartesianProductIterator(candidates)
    var terms = Set[Formula]()
    while (tuplified.hasNext) {
      val ts = tuplified.next
      if (modifiers.forall(_(ts))) {
        val map = vars.zip(ts).toMap[Formula,Formula]
        val t = FormulaUtils.map( f => map.getOrElse(f, f), expr)
        if (!gts.contains(t)) {
          terms += t
        }
      }
    }
    terms
  }

}
