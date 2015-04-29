package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer

object CL extends CL( Some(2), //pairwise Venn regions
                      Some(Set(UnInterpreted("ProcessID"))) //only on set of type ProcessID
                    ) {
  
  val procType = UnInterpreted("ProcessID")
  val HO = UnInterpretedFct("HO",Some(procType ~> FSet(procType)))
  val n = Variable("n").setType(Int)

  def hasHO(f: Formula): Boolean = {
    def check(acc: Boolean, f: Formula) = f match {
      case Application(UnInterpretedFct("HO",_,_), _) => true
      case _ => acc
    }
    FormulaUtils.collect(false, check, f)
  }

}

object ClFull extends CL(None, None) 

class CL(bound: Option[Int],
         onType: Option[Set[Type]]) {

  //TODO generalize
  //-Map[A,B] as a Set[A] of keys and a content(key: A): B function

  import CL.{procType, HO, n, hasHO}

  protected def normalize(f: Formula) = {
    //TODO some (lazy) CNF conversion ?
    //TODO purification before or after instantiation ?
    val f1 = Simplify.normalize(f)
    val f2 = Simplify.nnf(f1)
    val f3 = Simplify.boundVarUnique(f2)
    f3
  }
 
  protected def keepAsIt(f: Formula): Boolean = {
    var hasComp = false
    def check(f1: Formula) = f1 match {
      case Comprehension(_, _) => hasComp = true
      case _ => ()
    }
    FormulaUtils.traverse(check, f)
    Quantifiers.isEPR(f) && !hasComp
  }

  protected def forall(f: Formula): Boolean = {
    var isForAll = false
    def check(f1: Formula) = f1 match {
      case ForAll(_, _) => isForAll = true
      case Comprehension(_, _) => isForAll=false
      case _ => ()
    }
    FormulaUtils.traverse(check, f)
    isForAll 
  }
  //make sure we have a least one process
  protected def getGrounTerms(fs: List[Formula]): Set[Formula] = {
    val gts0 = FormulaUtils.collectGroundTerms(And(fs:_*))
    if (gts0.exists( t => t.tpe == CL.procType)) gts0
    else gts0 + Variable(Namer("p")).setType(CL.procType)
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
    (f2, allDefs)
  }
  
  protected def collectHO(conjuncts: List[Formula]): List[SetDef] = {
    val gts = FormulaUtils.collectGroundTerms(And(conjuncts:_*))
    val hos = gts.filter{ case Application(UnInterpretedFct("HO",_,_), _) => true
                          case _ => false } 
    hos.toList.map( ho => SetDef(Set(), ho, None) )
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

  //TODO non-empty scope means we should introduce more terms
  def reduceComprehension(conjuncts: List[Formula],
                          cClasses: CongruenceClasses = new CongruenceClasses(Nil, Map.empty), univConjuncts: List[Formula]=Nil): List[Formula] = {

    //get the comprehensions and HO sets from the formula
    val (_woComp, _c1) = collectComprehensionDefinitions(conjuncts)
    val ho = collectHO(conjuncts)
    val (c1, subst) = SetDef.normalize(_c1 ++ ho, cClasses)
    val woComp = _woComp.map(FormulaUtils.map( f => subst.getOrElse(f, f), _))
    Logger("CL", Debug, "similar: " + subst.mkString(", "))

    //generate the ILP
    val byType = c1.groupBy(_.contentTpe)
    val ilps =
      for ( (tpe, sDefs) <- byType if onType.map(_ contains tpe).getOrElse(true)) yield {
        Logger("CL", Info, sDefs.mkString("reduceComprehension "+tpe+"\n    ","\n    ",""))
        val fs = sDefs.map(_.fresh)
        val sets = fs.map( sd => (sd.id, sd.body)) 
        val cstrs = bound match {
          case Some(b) => new VennRegionsWithBound(b, tpe, sizeOfUniverse(tpe), sets, univConjuncts).constraints
          case None => new VennRegions(tpe, sizeOfUniverse(tpe), sets, univConjuncts).constraints
        }
        val scope = fs.map(_.scope).flatten.toList
        ForAll(scope, cstrs) //TODO this needs skolemization
      }
    Lt(Literal(0), n) :: woComp ::: ilps.toList
  }
  
  protected def cleanUp(ls: List[Formula]) = {
    val f = And(ls:_*)
    val simp = Simplify.boundVarUnique(f)
    val qf = Quantifiers.skolemize(simp) //get ride of ∃
    val renamed = Simplify.deBruijnIndex(qf)
    Simplify.simplify(renamed)
  }
  
  def reduce(formula: Formula): Formula = {
    //TODO normalization:
    //-de Bruijn then bound var unique (TODO make sure there is no clash about this)
    //-filter type of the VennRegions
    //-congruence closure to reduce the number of terms instanciation

    val query = normalize(formula)
    assert(Typer(query).success, "CL.entailment, not well typed")

    //remove the top level ∃ quantifiers (sat query)
    val (query1, _) = Quantifiers.getExistentialPrefix(query)
    val clauses0 = FormulaUtils.getConjuncts(query1)
    val clauses = clauses0.map( f => {
      val f2 = Simplify.pnf(f)
      Quantifiers.fixUniquelyDefinedUniversal(f2)
    })

    val (epr, rest) = clauses.partition(keepAsIt)
    val (univ, rest1) = rest.partition(forall)
  
    Logger("CL", Debug, "epr clauses:\n  " + epr.mkString("\n  "))
    Logger("CL", Debug, "clauses to process:\n  " + rest.mkString("\n  "))
    //CD add neprUniv 	
    //get rid on the ∀ quantifiers
    val cCls0 = CongruenceClosure(clauses)
    //separte groud term into equivalence classes 
    val gts0 = getGrounTerms(epr)
    val inst0 = FormulaUtils.getConjuncts(InstGen.saturate(And(rest:_*), gts0, cCls0, Some(0), false))
	    
    //the venn regions
    val cCls1 = CongruenceClosure(epr ++ inst0)
    val withILP = epr ::: CL.reduceComprehension(inst0, cCls1, univ)
    
    //add axioms for the other theories
    val withSetAx = SetOperationsAxioms.addAxioms(withILP)
    val withOpt = OptionAxioms.addAxioms(withSetAx)
    val withTpl = TupleAxioms.addAxioms(withOpt)


    //clean-up and skolemization
    val last = cleanUp(withTpl)
    assert(Typer(last).success, "CL.reduce, not well typed")
    last
  }
  
  def entailment(hypothesis: Formula, conclusion: Formula): Formula = {
    reduce(And(hypothesis, Not(conclusion)))
  }
  
}
