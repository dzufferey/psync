package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer

//Would it be better to instantiate the universal on the ground terms and then apply the procedure ?
//Some base for local theories and other such things  
//we need:
// * e-matching
// * congurence closure, equivalence classes
// * term generator (for ψ-locality)
// * flattening of axioms (introduce some ⇒)
// * set of ground terms
// * namer for hygiene

/** Instance generation: methods to instanciate the quantifiers */
object InstGen {

  //TODO groundTerms should be up to equalities / equivalence classes

  /** instantiate the given free variable with the provided gounded terms (substitution) */
  def instantiateWithTerms(v: Variable, axiom: Formula, groundTerms: Set[Formula], local: Boolean = false): List[Formula] = {
    if (local) {
      ??? //TODO local instantiation needs fetching the fct up to boolean level and implementing E-matching
    } else {
      val candidates = groundTerms.filter(_.tpe == v.tpe).toList
      candidates.toList.map( gt => FormulaUtils.map(x => if (x == v) gt else x, axiom) )
    }
  }

  //the returned formula still contains ∃ quantifiers
  protected def instantiateGlobally(formula: Formula, groundTerms: Set[Formula]): Formula = formula match {
    case Not(arg) =>
      Not(instantiateGlobally(arg, groundTerms))
    case Or(args @ _*) =>
      val args2 = args.map(instantiateGlobally(_, groundTerms))
      Or(args2:_*)
    case And(args @ _*) =>
      val args2 = args.map(instantiateGlobally(_, groundTerms))
      And(args2:_*)
    case Exists(vs, f) =>
      Exists(vs, instantiateGlobally(f, groundTerms))
    case ForAll(vs, f) =>
      val fs = vs.foldLeft(List(f))( (acc, v) =>
        acc.flatMap(instantiateWithTerms(v, _, groundTerms, false))
      )
      And(fs.map(instantiateGlobally(_, groundTerms)):_*)
    case other => other
  }
  
  //TODO E-matching
  protected def instantiateLocally(formula: Formula, groundTerms: Set[Formula]): Formula = formula match {
    case _ => ???
  }

  def postprocess(f: Formula): Formula = {
    val simp = Simplify.boundVarUnique(f)
    val qf = Quantifiers.skolemize(simp)
    FormulaUtils.flatten(qf)
  }

  /** instantiate all the universally quantified variables with the provided ground terms.
   * @param groundTerms set of grounds terms used to instantiate the variables
   * @param local should the instantiation be local, i.e., not generating new ground terms
   */
  def saturateOver( axiom: Formula,
                    groundTerms: Set[Formula],
                    local: Boolean ): Formula = {
    val inst = if (local) instantiateLocally(axiom, groundTerms)
               else instantiateGlobally(axiom, groundTerms)
    postprocess(inst)
  }

  /** instantiate all the universally quantified variables with the provided ground terms.
   * @param formula list of formula
   * @param groundTerms (optional) set of terms to add to the terms present in the formulas
   * @param depth optional bound on the recursion depth (depth 0 does not intruduce new ground terms)
   * @param local (optional) should the instantiation be local (default is false)
   */
  def saturate( formula: Formula,
                groundTerms: Set[Formula] = Set(),
                depth: Option[Int] = None,
                local: Boolean = false): Formula = {
    val gts = groundTerms ++ FormulaUtils.collectGroundTerms(formula)
    val formula2 = if (local) instantiateLocally(formula, gts)
                   else instantiateGlobally(formula, gts)
    if (depth.getOrElse(1) <= 0) {
      postprocess(formula2)
    } else {
      val gts2 = FormulaUtils.collectGroundTerms(formula2) -- gts
      if (gts2.isEmpty) {
        postprocess(formula2)
      } else {
        saturate(formula, gts ++ gts2, depth.map(_ - 1), local)
      }
    }
  }
   

}
