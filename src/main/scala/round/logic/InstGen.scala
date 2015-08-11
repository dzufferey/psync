package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer


/** Instance generation: methods to instanciate the quantifiers */
object InstGen {

  def postprocess(f: Formula): Formula = {
    val simp = Simplify.boundVarUnique(f)
    val qf = Quantifiers.skolemize(simp)
    Simplify.simplifyBool(FormulaUtils.flatten(qf))
  }

  protected def toCongruenceClosure(cClasses: CC) = cClasses.mutable
  
  /** instantiate all the universally quantified variables with the provided ground terms.
   * @param formula list of formula
   * @param mandatoryTerms given an chain of quantified variables, at least one of them will be instantiated with a mandatory term (or its representative in cClasses). The others may also use the optionalTerms.
   * @param depth (optional) bound on the recursion depth, depth == 0 is equivalent to local instantiation
   * @param cClasses (optional) congruence classes to reduce the number of terms used in the instantiation
   * @param additionalTerms (optional) set of terms to add to the terms present in the formulas
   */
  def saturateWith( formula: Formula,
                    mandatoryTerms: Set[Formula],
                    depth: Option[Int] = None,
                    cClasses: CC = CongruenceClasses.empty,
                    additionalTerms: Set[Formula] = Set()): Formula = {
    //get an incremental CC
    val cc = toCongruenceClosure(cClasses)
    //push all the terms to be sure
    mandatoryTerms.foreach(cc.repr)
    additionalTerms.foreach(cc.repr)
    FormulaUtils.collectGroundTerms(formula).foreach(cc.repr)
    //make sure formula is taking into account
    cc.addConstraints(formula)

    val (ax, rest) = FormulaUtils.getConjuncts(formula).partition(Quantifiers.hasFAnotInComp)
    val gen = new IncrementalGenerator(And(ax:_*), cc)
    val mRepr = mandatoryTerms.map(cc.repr)
    //ignore things without mandatoryTerms
    cc.groundTerms.view.map(cc.repr).filterNot(mRepr).foreach(gen.generate)
    //TODO this wram-up gets very slow when there are many groundTerms, would it be possible to clone an incremental generator ?

    //saturate with the remaining terms
    val insts = gen.saturate(depth)
    val res = And(rest ++ insts :_*)
    postprocess(res)
  }

  /** instantiate all the universally quantified variables with the provided ground terms.
   * @param formula list of formula
   * @param depth (optional) bound on the recursion depth, depth == 0 is equivalent to local instantiation
   * @param cClasses (optional) congruence classes to reduce the number of terms used in the instantiation
   * @param additionalTerms (optional) set of terms to add to the terms present in the formulas/cClasses
   */
  def saturate( formula: Formula,
                depth: Option[Int] = None,
                cClasses: CC = new CongruenceClosure,
                additionalTerms: Set[Formula] = Set()): Formula = {
    val cc = toCongruenceClosure(cClasses)
    additionalTerms.foreach(cc.repr) //make sure all the terms are in cc
    cc.addConstraints(formula)
    val (ax, rest) = FormulaUtils.getConjuncts(formula).partition(Quantifiers.hasFAnotInComp)
    val gen = new IncrementalGenerator(And(ax:_*), cc)
    val insts = gen.saturate(depth)
    And(rest ++ insts :_*)
  }
 
}
