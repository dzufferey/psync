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
                    expr: Formula,
                    modifiers: (Seq[Formula] => Boolean)*) {

  val symbols = FormulaUtils.collectSymbols(expr)

  //TODO a more efficient version that can be used in InstGen
  //normalizing, chaining, etc
  //watchlist, avoiding duplication, etc.

  //returns new terms, i.e., not already in gts
  def apply(gts: Set[Formula]): Set[Formula] = {
    //TODO this is the brain-dead version...
    val candidates = vars.map( v => gts.filter( t => {
      t.tpe == v.tpe &&
      FormulaUtils.collectSymbols(t).intersect(symbols).isEmpty
    }))
    val tuplified = Misc.cartesianProduct(candidates).filter( cs => modifiers.forall(_(cs)) )
    val terms = tuplified.foldLeft(Set[Formula]())( (acc, ts) => {
      val map = vars.zip(ts).toMap[Formula,Formula]
      val t = FormulaUtils.map( f => map.getOrElse(f, f), expr)
      if (gts contains t) acc else acc + t
      acc + t
    })
    terms
  }

}
