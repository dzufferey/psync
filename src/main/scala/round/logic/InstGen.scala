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
   * @param axioms a list of axioms
   * @param cClasses (optional) congruence classes to reduce the number of terms used in the instantiation
   * @param additionalTerms (optional) set of terms to add to the terms present in the formulas
   */
  def makeGenerator( axioms: Formula,
                     cClasses: CC = new CongruenceClosure,
                     additionalTerms: Iterable[Formula] = Nil) = {
    val cc = cClasses.mutable
    //push all the terms to be sure
    additionalTerms.foreach(cc.repr)
    FormulaUtils.collectGroundTerms(axioms).foreach(cc.repr)
    //make sure formula is taken into account
    cc.addConstraints(axioms)
    //
    new IncrementalGenerator(axioms, cc)
  }
  
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
    val gen = makeGenerator(formula, cClasses, mandatoryTerms ++ additionalTerms)
    val cc = gen.cc
    val mRepr = mandatoryTerms.map(cc.repr)
    //ignore things without mandatoryTerms
    cc.groundTerms.view.map(cc.repr).filterNot(mRepr).foreach(gen.generate)

    //saturate with the remaining terms
    val insts = gen.saturate(depth)
    val res = And(gen.leftOver ++ insts :_*)
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
    val gen = makeGenerator(formula, cClasses, additionalTerms)
    val insts = gen.saturate(depth)
    And(gen.leftOver ++ insts :_*)
  }
 
}
