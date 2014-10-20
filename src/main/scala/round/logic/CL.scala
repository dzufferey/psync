package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object CL {

  protected def normalize(f: Formula) = {
    //TODO some CNF conversion ?
    //TODO purification before or after instantiation ?
    val f1 = Simplify.normalize(f)
    val f2 = Simplify.nnf(f1)
    val f3 = Simplify.boundVarUnique(f2)
    f3
  }

  //unsupported quantifiers are implicitely universal, we care about the ∀ in ∀∃φ
  protected def getUnsupportedQuantifierPrefix(f: Formula): (Formula, List[Variable]) = {
    val f2 = Simplify.pnf(f)
    //the only forall we support are ProcessID
    //should still allows EPR no quantified processID below
    //...
    ???
  }

  /** preprocess and reduce (hypothesis ∧ ¬conclusion),
   *  returned formula can be checked for satisfiability. */
  def entailment(hypothesis: Formula, conclusion: Formula): Formula = {
    val h1 = normalize(hypothesis)
    val c1 = normalize(Not(conclusion))

    val (h2, ext) = Quantifiers.getExistentialPrefix(h1)
    val gt = FormulaUtils.collectGroundTerms(h2)
    //what can be used to instantiate the unsupported quantifiers: ext ∪ gt

    val cs1 = FormulaUtils.getConjunts(c1)
    //TODO matching quantifier and then call satisfiable
    val cs2 = cs1.flatMap( c => {
      val (qf, vs) = getUnsupportedQuantifierPrefix(c)
      ???
    })

    Logger("CL", Warning, "TODO!!")
    ???
  }
  
  def reduce(fomula: Formula): Formula = {
    Logger("CL", Warning, "TODO!!")
    ???
  }
  
}
