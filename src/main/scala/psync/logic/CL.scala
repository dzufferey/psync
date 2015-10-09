package psync.logic

import psync.formula._
import psync.logic.quantifiers._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer

object CL {

  val procType = UnInterpreted("ProcessID")
  val timeType = UnInterpreted("Time")
  val HO = UnInterpretedFct("HO",Some(procType ~> FSet(procType)))
  val n = Variable("n").setType(Int)

  def hasHO(f: Formula): Boolean = {
    def check(f: Formula) = f match {
      case Application(UnInterpretedFct("HO",_,_), _) => true
      case _ => false
    }
    FormulaUtils.exists(check, f)
  }

  def hasComp(f: Formula) = {
    def check(f1: Formula) = f1 match {
      case Comprehension(_, _) => true
      case _ => false
    }
    FormulaUtils.exists(check, f)
  }
 
  def keepAsIt(f: Formula): Boolean = {
    //TODO this accepts formula that would be rejected if they were skolemized!!
    !hasComp(f) && TypeStratification.isStratified(f)
    //!hasComp && Quantifiers.isEPR(f)
  }

}


class CL(config: ClConfig) {

  import CL._

  protected def bound = config.vennBound
  protected def onType = config.onType

  protected def normalize(f: Formula) = {
    //TODO some (lazy) CNF conversion ?
    //TODO purification before or after instantiation ?
    val f1 = Simplify.normalize(f)
    val f2 = Simplify.nnf(f1)
    val f3 = FormulaUtils.flatten(f2)
    val f4 = Simplify.boundVarUnique(f3)
    val f5 = Simplify.mergeExists(f4)
    val f6 = Simplify.splitTopLevelForall(f5)
    f6
  }

  protected def forallOnly(f: Formula): Boolean = {
    var isForAll = false
    var hasComp = false
    def check(f1: Formula) = f1 match {
      case ForAll(_, _) => isForAll = true
      case Comprehension(_, _) => hasComp = true
      case _ => ()
    }
    FormulaUtils.traverse(check, f)
    isForAll && !hasComp
  }

  //TODO assumes positive occurance!!
  protected def namedComprehensions(conjuncts: List[Formula]): (List[Formula], Set[SetDef]) = {
    var acc = Set[SetDef]()
    def process(bound: Set[Variable], f: Formula) = f match {
      case Eq(id, c @ Binding(Comprehension, vs, body)) => 
        val scope = bound intersect (body.freeVariables -- vs)
        acc += SetDef(scope, id, Some(c))
        True()
      case Eq(c @ Binding(Comprehension, vs, body), id) => 
        val scope = bound intersect (body.freeVariables -- vs)
        acc += SetDef(scope, id, Some(c))
        True()
      case other =>
        other
    }
    val f2 = FormulaUtils.mapWithScope(process, And(conjuncts:_*))
    (FormulaUtils.getConjuncts(f2), acc)
  }
  
  //TODO something is wrong here
  protected def anonymComprehensions(conjuncts: List[Formula]): (List[Formula], Set[SetDef]) = {
    //reuse defs when possible
    var acc = Set[SetDef]()
    def process(bound: Set[Variable], f: Formula) = f match {
      case c @ Binding(Comprehension, vs, body) => 
        val scope = bound intersect (body.freeVariables -- vs)
        val tpe = c.tpe match {
          case t @ FSet(_) => t
          case other =>
            val t = FSet(vs.head.tpe)
            Logger("CL", Warning, "Comprehension with type " + other + " instead of " + t + "\n  " + c)
            Logger.assert(vs.size == 1, "CL", "Comprehension not binding just one var " + vs)
            t
        }
        val id = Quantifiers.skolemify(Variable(Namer("_comp")).setType(tpe), scope)
        val sd = SetDef(scope, id, Some(c)).normalize
        val id2 = acc.find( d => d.similar(sd) ) match {
          case Some(d) =>
            d.id
          case None =>
            acc += sd
            id
        }
        id2
      case other =>
        other
    }
    val f2 = FormulaUtils.mapWithScope(process, And(conjuncts:_*))
    (FormulaUtils.getConjuncts(f2), acc)
  }

  protected def collectComprehensionDefinitions(conjuncts: List[Formula]): (List[Formula], Set[SetDef]) = {
    val (f1, defs1) = namedComprehensions(conjuncts)
    val (f2, defs2) = anonymComprehensions(f1)
    val allDefs = (defs1 ++ defs2).map(_.normalize)
    assert(allDefs.forall(_.scope.isEmpty), "non-empty scope")
    (f2, allDefs)
  }
  
  protected def collectSetTerms(gts: Set[Formula]): List[SetDef] = {
    val sts = gts.filter( _.tpe match { case FSet(_) => true
                                        case _ => false } )
    sts.toList.map( ho => SetDef(ho, None) )
  }

  protected def sizeOfUniverse(tpe: Type): Option[Formula] = tpe match {
    case `procType` => Some(n)
    case Bool => Some(Literal(2))
    case Product(args) =>
      val s2 = args.map(sizeOfUniverse)
      if (s2.forall(_.isDefined)) {
        if (s2.isEmpty) Some(Literal(1))
        else Some(Times(s2.map(_.get):_*))
      } else {
        None
      }
    case _ => None
  }
  
  //assumes that conjuncts is already added to gen.cc
  def reduceComprehension(conjuncts: List[Formula],
                          gen: Generator): List[Formula] = {
    //get the comprehensions definitions and normalize
    val (woComp, _c1) = collectComprehensionDefinitions(conjuncts)
    val (c1, subst) = SetDef.normalize(_c1, gen.cc)
    val newEqs = subst.map{ case (v1, v2) => Eq(v1, v2) }.toList
    Logger("CL", Debug, "similar: " + subst.mkString(", "))
    newEqs.foreach(gen.cc.addConstraints)
    //get all the sets and merge the ones which are equal
    val gts = FormulaUtils.collectGroundTerms(And(conjuncts:_*))
    val _c2 = c1 ++ collectSetTerms(gts)
    val c2 = SetDef.mergeEqual(_c2, gen.cc)
    //generate the ILP
    val byType = c2.groupBy(_.contentTpe)
    val ilps =
      for ( (tpe, sDefs) <- byType if onType.map(_ contains tpe).getOrElse(true)) yield {
        Logger("CL", Info, sDefs.mkString("reduceComprehension "+tpe+" (nbr = " +sDefs.size+ ")\n    ","\n    ",""))
        val fs = sDefs.map(_.fresh)
        val sets = fs.map( sd => (sd.id, sd.body)) 
        val cstrs = bound match {
          case Some(b) => VennRegions.withBound(b, tpe, sizeOfUniverse(tpe), sets, gen, false)
          case None => VennRegions(tpe, sizeOfUniverse(tpe), sets, gen)
        }
        val scope = fs.map(_.scope).flatten.toList
        ForAll(scope, cstrs)
      }
    Lt(Literal(0), n) :: newEqs ::: woComp ::: ilps.toList
  }

  def reduceComprehension(conjuncts: List[Formula],
                          cClasses: CC = CongruenceClasses.empty,
                          univConjuncts: List[Formula] = Nil): List[Formula] = {
    val gen = InstGen.makeGenerator(And(univConjuncts:_*), cClasses)
    cClasses.groundTerms.foreach(gen.generate) //warm-up the generator
    reduceComprehension(conjuncts, gen)
  }
  
  protected def cleanUp(ls: List[Formula]) = {
    val f = And(ls:_*)
    val simp = Simplify.boundVarUnique(f)
    val qf = Quantifiers.skolemize(simp) //get ride of ∃
    val renamed = Simplify.deBruijnIndex(qf)
    Simplify.simplify(renamed)
  }

  protected def quantifierInstantiation(fs: List[Formula], cc: CongruenceClosure): (List[Formula], Generator) = {
    Logger("CL", Debug, "instantiation strategy: " + config.instantiationStrategy)
    config.instantiationStrategy match {
      case Eager(bnd, local) =>
        val (epr, rest) = fs.partition(keepAsIt)
        Logger("CL", Debug, "epr/stratified clauses:\n  " + epr.mkString("\n  "))
        Logger("CL", Debug, "clauses to process:\n  " + rest.mkString("\n  "))
        val gen = InstGen.makeGenerator(And(rest:_*), cc)
        val res = epr ::: gen.leftOver ::: gen.saturate(bnd, local) //leftOver contains things not processed by the generator
        //gen.log(Debug)
        (res, gen)
      case Guided(bnd, local) =>
        Logger("CL", Debug, "clauses to process:\n  " + fs.mkString("\n  "))
        val gen = InstGen.makeGuidedGenerator(And(fs:_*), cc)
        val res = gen.leftOver ::: gen.saturate(bnd, local) //leftOver contains things not processed by the generator
        //gen.log(Debug)
        (res, gen)
    }
  }
  
  def reduce(formula: Formula): Formula = {
    //TODO make that part more modular:
    //preprocessing:
    //  term generation for 'unsupported' ∀
    //  instantiate not in one step but 
    //filtering for venn region:
    //  something smarter than the type ?

    val query = normalize(formula)
    assert(Typer(query).success, "CL.reduce, not well typed")

    //remove the top level ∃ quantifiers (sat query)
    val (query1, _) = Quantifiers.getExistentialPrefix(query)
    val clauses0 = FormulaUtils.getConjuncts(query1)
    val clauses = clauses0.map( f => {
      val f2 = Simplify.pnf(f)
      Quantifiers.fixUniquelyDefinedUniversal(f2)
    })
    
    val cc = new CongruenceClosure //incremental CC
    cc.addConstraints(clauses)
    //make sure we have a least one process
    if (cc.groundTerms.forall(_.tpe != procType)) {
      cc.repr(Variable(Namer("p")).setType(procType))
    }
    //Logger("CL", Debug, "CC is\n" + cc)

    val (inst, gen) = quantifierInstantiation(clauses, cc)
    Logger("CL", Debug, "after instantiation:\n  " + inst.mkString("\n  "))

    //generate keySet for Maps if they are not already there
    ReduceMaps.addMapGroundTerms(cc)
    
    //the venn regions
    val withILP = reduceComprehension(inst, gen) //TODO this generate quite a bit more terms!
    
    //add axioms for the other theories
    val withSetAx = SetOperationsAxioms.addAxioms(withILP)
    val withOpt = OptionAxioms.addAxioms(withSetAx)
    val withTpl = TupleAxioms.addAxioms(withOpt)
    val withoutTime = ReduceTime(withTpl)
    val expendedLt = ReduceOrdered(withoutTime)


    //clean-up and skolemization
    val last = cleanUp(expendedLt)
    //assert(Typer(last).success, "CL.reduce, not well typed")
    last
  }
  
  def entailment(hypothesis: Formula, conclusion: Formula): Formula = {
    reduce(And(hypothesis, Not(conclusion)))
  }
  
}
