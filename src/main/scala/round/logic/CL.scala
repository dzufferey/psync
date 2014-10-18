package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object CL {

  protected def normalize(f: Formula) = {
    //TODO some CNF conversion ?
    //TODO drop existential quantifier prefix
    //TODO purification before or after instantiation ?
    Simplify.nnf(Simplify.normalize(f))
  }

  //unsupported quantifiers are implicitely universal
  //TODO if there is multiple candidated for instantiations: we need to skolemize ...
  def getUnsupportedQuantifierPrefix(f: Formula): (Formula, List[Variable]) = {
    ???
  }

  /** preprocess and reduce (hypothesis ∧ ¬conclusion),
   *  returned formula can be checked for satisfiability. */
  def entailment(hypothesis: Formula, conclusion: Formula): Formula = {
    val h1 = normalize(hypothesis)
    val c1 = normalize(Not(conclusion))

    //TODO matching quantifier and then call satisfiable
    val gt = FormulaUtils.collectGroundTerms(h1) //what can be used to instantiate the unsupported quantifiers
    val cConjuncts = FormulaUtils.getConjunts(c1).map(Simplify.pnf)

    Logger("CL", Warning, "TODO!!")
    ???
  }
  
  def reduce(fomula: Formula): Formula = {
    Logger("CL", Warning, "TODO!!")
    ???
  }
  
}
