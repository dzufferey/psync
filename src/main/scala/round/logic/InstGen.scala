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

//TODO
// more efficient implementation
// better simplification:
//  redundant terms and formula being generated multiple times

/** Instance generation: methods to instanciate the quantifiers */
object InstGen {

  /** instantiate the given free variable with the provided gounded terms (substitution) */
  protected def instantiateWithTerms(v: Variable, axiom: Formula, groundTerms: Set[Formula]): List[Formula] = {
    val candidates = groundTerms.filter(_.tpe == v.tpe).toList
    Logger("InstGen", Debug, "instantiating "+ v +" with " + candidates.mkString(", "))
    candidates.toList.map( gt => FormulaUtils.replace(v, gt, axiom) )
  }

  //the returned formula still contains ∃ quantifiers
  protected def instantiateGlobally(formula: Formula, groundTerms: Set[Formula]): Formula =
    instantiateGlobally(formula, Set(), true, groundTerms)
  
  /* the returned formula still contains ∃ quantifiers
   * if (didMandatory) assume mandatoryTerms ⊆ groundTerms
   */
  protected def instantiateGlobally(formula: Formula,
                                    mandatoryTerms: Set[Formula],
                                    didMandatory: Boolean,
                                    groundTerms: Set[Formula]): Formula = formula match {
    case Not(arg) =>
      Not(instantiateGlobally(arg, mandatoryTerms, didMandatory, groundTerms))
    case Or(args @ _*) =>
      val args2 = args.map(instantiateGlobally(_, mandatoryTerms, didMandatory, groundTerms))
      Or(args2:_*)
    case And(args @ _*) =>
      val args2 = args.map(instantiateGlobally(_, mandatoryTerms, didMandatory, groundTerms))
      val args3 = args2.flatMap(FormulaUtils.getConjuncts)
      And(args3:_*)
    case Exists(vs, f) =>
      Exists(vs, instantiateGlobally(f, mandatoryTerms, didMandatory, groundTerms))
    case ForAll(v :: vs, f) =>
      val f2 = if (groundTerms.forall(_.tpe != v.tpe)) True()
               else instantiateGlobally(ForAll(vs, f), mandatoryTerms, didMandatory, groundTerms)
      if (didMandatory) {
        And(instantiateWithTerms(v, f2, groundTerms):_*)
      } else {
        //case 1: now
        val candidates = mandatoryTerms.filter(_.tpe == v.tpe).toList
        val f2Now = if (candidates.isEmpty) True()
                    else instantiateGlobally(ForAll(vs, f), Set(), true, mandatoryTerms ++ groundTerms)
        val f3Now = candidates.toList.map( gt => FormulaUtils.replace(v, gt, f2Now) )
        //case 2: not now
        val f3NotNow = instantiateWithTerms(v, f2, groundTerms)
        //
        And((f3Now ++ f3NotNow):_*)
      }
    case ForAll(Nil, f) =>
      if (!didMandatory && FormulaUtils.universallyBound(f).isEmpty) True()
      else instantiateGlobally(f, mandatoryTerms, didMandatory, groundTerms)
    case other => other
  }
  
  //TODO E-matching
  protected def instantiateLocally( formula: Formula,
                                    groundTerms: Set[Formula],
                                    cClasses: CongruenceClasses = new CongruenceClasses(Nil, Map.empty)
                                  ): Formula = formula match {
    case _ => ???
  }

  def postprocess(f: Formula): Formula = {
    val simp = Simplify.boundVarUnique(f)
    val qf = Quantifiers.skolemize(simp)
    FormulaUtils.flatten(qf)
  }

  //at least one variable must be instantiated with one of the new term
  protected def instanciateOneStep( formula: Formula,
                                    mandatoryTerms: Set[Formula],
                                    gts: Set[Formula],
                                    cClasses: CongruenceClasses,
                                    local: Boolean ): Formula = {
    if (local) {
      //TODO mandatoryTerms
      instantiateLocally(formula, gts, cClasses)
    } else {
      val mRepr = mandatoryTerms.map(cClasses.repr(_))
      val gRepr = gts.map(cClasses.repr(_)) -- mRepr
      instantiateGlobally(formula, mRepr, false, gRepr)
    }
  }
  
  /** instantiate all the universally quantified variables with the provided ground terms.
   * @param formula list of formula
   * @param mandatoryTerms given an chain of quantified variables, at least one of them will be instantiated with a mandatory term. The others may also use the optionalTerms
   * @param optionalTerms (optional) set of terms to add to the terms present in the formulas
   * @param cClasses (optional) congruence classes to reduce the number of terms used in the instantiation
   * @param depth (optional) bound on the recursion depth (depth 0 does not intruduce new ground terms)
   * @param local (optional) should the instantiation be local (default is false)
   */
  def saturateWith( formula: Formula,
                    mandatoryTerms: Set[Formula],
                    optionalTerms: Set[Formula] = Set(),
                    cClasses: CongruenceClasses = CongruenceClasses.empty,
                    depth: Option[Int] = None,
                    local: Boolean = false ): Formula = {
    val gts = mandatoryTerms ++ optionalTerms ++ FormulaUtils.collectGroundTerms(formula)
    //ground terms are from the epr part 
    val formula2 = instanciateOneStep(formula, mandatoryTerms, gts, cClasses, local)
    if (depth.getOrElse(1) <= 0) {
      postprocess(formula2)
    } else {
      val gts2 = FormulaUtils.collectGroundTerms(formula2) -- gts
      if (gts2.isEmpty) {
        postprocess(formula2)
      } else {
        val cClasses1 = CongruenceClosure(And(formula2, cClasses.formula))
        saturateWith(formula, gts2, gts, cClasses1, depth.map(_ - 1), local)
      }
    }
  }

  /** instantiate all the universally quantified variables with the provided ground terms.
   * @param formula list of formula
   * @param groundTerms (optional) set of terms to add to the terms present in the formulas
   * @param cClasses (optional) congruence classes to reduce the number of terms used in the instantiation
   * @param depth (optional) bound on the recursion depth (depth 0 does not intruduce new ground terms)
   * @param local (optional) should the instantiation be local (default is false)
   */
  def saturate( formula: Formula,
                groundTerms: Set[Formula] = Set(),
                cClasses: CongruenceClasses = CongruenceClasses.empty,
                depth: Option[Int] = None,
                local: Boolean = false ): Formula = {
    val gts = groundTerms ++ FormulaUtils.collectGroundTerms(formula)
    saturateWith(formula, gts, Set(), cClasses, depth, local)
  }
 
}
