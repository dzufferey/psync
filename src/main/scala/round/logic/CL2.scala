package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer

//the constraints should be parametric on a bound: number of sets at the same time for the Venn regions
//
//forget about the scope, assume that the sets are terms
//either we can instantiate ∀ upfront to have enough ground sets (unfolding approach)
//or we can put the scope a posteriori (wrap the reduction in a module that takes care of the scope)
//
//Map:
//-introduce the set of keys
//-transfer the cardinality cstr to the set of keys
//-...
//
//one module for the cardinality cstr
//one module for the generation of sets combination
//one module to extract the comprehensions from the formula

//similar goal as CL but different pre-processing

object CL2 {

  def keepAsIt(f: Formula): Boolean = {
    var hasComp = false
    def check(f1: Formula) = f1 match {
      case Comprehension(_, _) => hasComp = true
      case _ => ()
    }
    FormulaUtils.traverse(check, f)
    Quantifiers.isEPR(f) && hasComp
  }

  //make sure we have a least one process
  def getGrounTerms(fs: List[Formula]): Set[Formula] = {
    val gts0 = FormulaUtils.collectGroundTerms(And(fs:_*))
    if (gts0.exists( t => t.tpe == CL.procType)) gts0
    else gts0 + Variable(Namer("p")).setType(CL.procType)
  }
  
  def reduce(formula: Formula): Formula = {
    val query = CL.normalize(formula)
    assert(Typer(query).success, "CL.entailment, not well typed")
    val (query1, _) = Quantifiers.getExistentialPrefix(query)
    val clauses = FormulaUtils.getConjuncts(query1)
    val (epr, rest) = clauses.partition(keepAsIt)
    val gts0 = getGrounTerms(epr)
    val inst0 = FormulaUtils.getConjuncts(InstGen.saturate(And(rest:_*), gts0, Some(0), false))
    val withILP = epr ::: CL.reduceComprehension(inst0) //TODO bound on the venn region
    val withSetAx = SetOperationsAxioms.addAxioms(withILP)
    val withOpt = OptionAxioms.addAxioms(withSetAx)
    val withTpl = TupleAxioms.addAxioms(withOpt)
    val last = InstGen.postprocess(And(withTpl:_*))
    assert(Typer(last).success, "CL.reduce, not well typed")
    last
  }

  
  def entailment(hypothesis: Formula, conclusion: Formula): Formula = {
    reduce(And(hypothesis, Not(conclusion)))
  }

}
