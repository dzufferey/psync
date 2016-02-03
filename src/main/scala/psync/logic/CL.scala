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
    //!hasComp(f) && Quantifiers.isEPR(f)
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

  protected def makeVennILP(defs: Iterable[SetDef], gen: Generator) = {
    val byType = defs.groupBy(_.contentTpe)
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
  }
  
  def reduceComprehension(conjuncts: List[Formula],
                          symbols: Map[Symbol,Formula],
                          gen: Generator): List[Formula] = {
    //generate keySet for Maps if they are not already there
    val keySets = ReduceMaps.newTerms(gen.cc)
    for (ks <- keySets) gen.cc.repr(ks) //add the new sets to the CC
    //collect all the sets
    val sets = gen.cc.groundTerms.filter( _.tpe match { case FSet(_) => true; case _ => false } )
    //instantiate the symbol the definitions
    val allDefs = sets.map{
      case a @ Application(sym, args) if symbols contains sym =>
        symbols(sym) match {
          case ForAll(_, Eq(Application(s2, args2), c @ Comprehension(_, _))) =>
            val subst = args2.zip(args).toMap
            SetDef(a, Some(FormulaUtils.map(f => subst.getOrElse(f,f), c).asInstanceOf[Binding]))
          case Eq(Application(s2, Nil), c @ Comprehension(_, _)) =>
            SetDef(a, Some(c.asInstanceOf[Binding]))
          case other =>
            Logger.logAndThrow("CL", Error, "expected set definition, found: " + other)
        }
      case other =>
        SetDef(other, None)
    }
    //merge if multiple definitions are equal,
    val setDefs = SetDef.mergeEqual(allDefs, gen.cc)
    //make the ILP
    val ilps = makeVennILP(setDefs, gen)
    Logger("CL", Debug, "ilps:\n  " + ilps.mkString("\n  "))
    Lt(Literal(0), n) :: ilps.toList ::: conjuncts
  }
  
  protected def cleanUp(ls: List[Formula]) = {
    val f = And(ls:_*)
    val simp = Simplify.boundVarUnique(f)
    val qf = Quantifiers.skolemize(simp) //get ride of ∃
    val renamed = Simplify.deBruijnIndex(qf)
    Simplify.simplify(renamed)
  }
  
  protected def quantifierInstantiation(strat: QStrategy, fs: List[Formula], cc: CongruenceClosure): (List[Formula], Generator) = {
    Logger("CL", Debug, "instantiation strategy: " + strat)
    strat match {
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
      case QSeq(fst, snd) =>
        val (fs1, _) = quantifierInstantiation(fst, fs, cc)
        val (fs2, g) = quantifierInstantiation(snd, fs, cc)
        ((fs1.toSet ++ fs2.toSet).toList, g)
      case QNew(t, bnd, local) =>
        val (axioms, leftOver) = fs.partition(Quantifiers.hasFAnotInComp)
        Logger("CL", Debug, "leftOver:\n  " + leftOver.mkString("\n  "))
        Logger("CL", Debug, "axioms:\n  " + axioms.mkString("\n  "))
        val gen = new IncrementalGenerator(axioms, t, cc)
        val generated = gen.saturate(bnd, local)
        Logger("CL", Debug, "generated: \n  " + generated.mkString("\n  "))
        val res = leftOver ::: generated
        //gen.log(Debug)
        (res, gen)
    }
  }

  protected def quantifierInstantiation(fs: List[Formula], cc: CongruenceClosure): (List[Formula], Generator) = {
    quantifierInstantiation(config.instantiationStrategy, fs, cc)
  }
  
  def reduce(formula: Formula): Formula = {

    val query = normalize(formula)
    assert(Typer(query).success, "CL.reduce, not well typed")
    
    //For comprehension
    var symbols = Map.empty[Symbol,Formula]

    //remove the top level ∃ quantifiers (sat query)
    val (query1, _) = Quantifiers.getExistentialPrefix(query)
    val clauses0 = FormulaUtils.getConjuncts(query1)
    val clauses = clauses0.map( f => {
      val f2 = Simplify.pnf(f)
      val f3 = Quantifiers.fixUniquelyDefinedUniversal(f2)
      val f4 = FormulaUtils.map({
        case c @ Comprehension(_, _) =>
          val (sym, d, args) = Quantifiers.symbolizeComprehension(c)
          symbols += (sym -> d)
          sym(args:_*)
        case other => other
      }, f3)
      Quantifiers.skolemize(f4)
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

    //the venn regions
    val withILP = reduceComprehension(inst, symbols, gen)
    
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
