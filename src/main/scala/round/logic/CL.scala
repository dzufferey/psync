package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer

object CL {

  //TODO generalize
  //-more than procType: to generate the constraints we need to know the size of the universe
  //-more than pairwise constraints
  //-Map[A,b] as a Set[A] of keys and a content(key: A): B function

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

  def normalize(f: Formula) = {
    //TODO some (lazy) CNF conversion ?
    //TODO purification before or after instantiation ?
    val f1 = Simplify.normalize(f)
    val f2 = Simplify.nnf(f1)
    val f3 = Simplify.boundVarUnique(f2)
    f3
  }

  //unsupported quantifiers are implicitely universal, we care about the ∀ in ∀∃φ
  //TODO should still allows EPR no quantified processID below
  protected def getUnsupportedQuantifierPrefix(f: Formula): (Formula, List[Variable]) = {
    val f2 = Simplify.pnf(f)
    val (f3,vs) = Quantifiers.getUniversalPrefix(f2)
    val (supported, unsupported) = vs.toList.partition( v => v.tpe == procType)
    Logger("CL", Info, "unsupported quantifiers are: " + unsupported.map(v => v.name + ": " + v.tpe).mkString(", "))
    val f4 = ForAll(supported, f3)
    (f4, unsupported)
  }


  protected def matchQuantifiers( qf: Formula, 
                                  vs: List[Variable],
                                  existentials: Iterable[Variable],
                                  groundTerms: Iterable[Formula]
                                ): Iterable[Formula] = {

    def findMatch(v: Variable) = {
      val prefered = existentials.filter(_.tpe == v.tpe)
      if (!prefered.isEmpty) {
        prefered
      } else {
        Logger("CL", Notice, "did not find instantiation candidate among existential for " + v + ": " + v.tpe)
        groundTerms.filter(_.tpe == v.tpe)
      }
    }

    vs.foldLeft(List(qf))( (acc, v) => {
      val candidates = findMatch(v)
      if (candidates.isEmpty) {
        Logger("CL", Notice, "did not find any instantiation candidate for " + v + ": " + v.tpe)
        acc.map( f => ForAll(List(v), f) )
      } else {
        Logger("CL", Info, "instantiating " + v + ": " + v.tpe + " with " + candidates.mkString(", "))
        acc.flatMap( f => InstGen.instantiateWithTerms(v, f, candidates.toSet) )
      }
    })
  }

  /** preprocess and reduce (hypothesis ∧ ¬conclusion),
   *  returned formula can be checked for satisfiability. */
  def entailment(hypothesis: Formula, conclusion: Formula): Formula = {
    assert(Typer(And(hypothesis, Not(conclusion))).success, "CL.entailment, not well typed")
    val h1 = normalize(hypothesis)
    val c1 = normalize(Not(conclusion))
    
    val (h2, ext) = Quantifiers.getExistentialPrefix(h1)
    val gt = FormulaUtils.collectGroundTerms(h2)
    //what can be used to instantiate the unsupported quantifiers: ext ∪ gt

    val c2 = {
      val (c, e) = Quantifiers.getExistentialPrefix(c1)
      //TODO this assumes unique bound var, we should alpha
      c
    }
    val cs1 = FormulaUtils.getConjuncts(c2)
    Logger("CL", Debug, "negated conclusion:\n " + cs1.mkString("\n "))
    val cs2 = cs1.flatMap( c => {
      val (qf, vs) = getUnsupportedQuantifierPrefix(c)
      matchQuantifiers(qf, vs, ext, gt)
    })
    val cs3 = cs2.map(Quantifiers.fixUniquelyDefinedUniversal)

    val query = cs3.foldLeft(h2)(And(_,_))
    reduce(query)
  }

  //the definition of a comprehension
  //TODO trim the scope when the body is defined
  case class SetDef(scope: Set[Variable], id: Formula, body: Option[Binding]) {
    def tpe = id.tpe
    def contentTpe = tpe match {
      case FSet(t) => t
      case _ => Logger.logAndThrow("CL", Error, "SetDef had not FSet type")
    }
    def fresh: SetDef = {
      val newScope = scope.map(v => v -> Variable(Namer(v.name)).setType(v.tpe)).toMap
      SetDef(newScope.values.toSet, id.alpha(newScope), body.map(_.alpha(newScope)))
    }
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
  
  protected def anonymComprehensions(conjuncts: List[Formula]): (List[Formula], Set[SetDef]) = {
    var acc = Set[SetDef]()
    def process(bound: Set[Variable], f: Formula) = f match {
      case c @ Binding(Comprehension, vs, body) => 
        val scope = bound intersect (body.freeVariables -- vs)
        val id = Quantifiers.skolemify(Variable(Namer("_comp")).setType(c.tpe), scope)
        acc += SetDef(scope, id, Some(c))
        id
      case other =>
        other
    }
    val f2 = FormulaUtils.mapWithScope(process, And(conjuncts:_*))
    (FormulaUtils.getConjuncts(f2), acc)
  }

  protected def collectComprehensionDefinitions(conjuncts: List[Formula]): (List[Formula], Set[SetDef]) = {
    val (f1, defs1) = namedComprehensions(conjuncts)
    val (f2, defs2) = anonymComprehensions(f1)
    (f2, defs1 ++ defs2)
  }

  protected def sizeOfUniverse(tpe: Type) = {
    if (tpe == procType) Some(n)
    else None
  }

  //TODO non-empty scope means we should introduce more terms
  def reduceComprehension(conjuncts: List[Formula]): List[Formula] = {
    val (woComp, c1) = collectComprehensionDefinitions(conjuncts)
    val v = Variable(Namer("v")).setType(procType)
    val ho = SetDef(Set(v), Application(HO, List(v)), None)
    val c2 = if (woComp exists hasHO) c1 + ho else c1
    val byType = c2.groupBy(_.contentTpe)
    Logger("CL", Debug,
      byType.mapValues(vs => vs.mkString("\n    ","\n    ","")).
        mkString("reduceComprehension, comprehensions:\n  ", "\n  ","")
    )
    val ilps =
      for ( (tpe, sDefs) <- byType ) yield {
        val fs = sDefs.map(_.fresh)
        val sets = fs.map( sd => (sd.id, sd.body)) 
        val vr = new VennRegions(tpe, sizeOfUniverse(tpe), sets)
        val cstrs = vr.constraints
        val scope = fs.map(_.scope).flatten.toList
        ForAll(scope, cstrs) //TODO this needs skolemization
      }
    Lt(Literal(0), n) :: woComp ::: ilps.toList
  }

  def reduce(formula: Formula): Formula = {
    val typed = Typer(formula).get 
    val n1 = normalize(typed)
    val n2 = Quantifiers.getExistentialPrefix(n1)._1
    val rawConjuncts = FormulaUtils.getConjuncts(n2)
    val conjuncts = rawConjuncts.map(f => Quantifiers.skolemize(Simplify.simplify(Simplify.pnf(f))))
    Logger("CL", Info, "reducing:\n  " + conjuncts.mkString("\n  "))
    val withILP = reduceComprehension(conjuncts)
    Logger("CL", Debug, "with ILP:\n  " + withILP.mkString("\n  "))
    val withSetAx = SetOperationsAxioms.addAxioms(withILP)
    val withOpt = OptionAxioms.addAxioms(withSetAx)
    val withTpl = TupleAxioms.addAxioms(withOpt)
    Logger("CL", Debug, "with axiomatized theories:\n  " + withTpl.mkString("\n  "))
    val last = withTpl
    Typer(And(last:_*)) match {
      case Typer.TypingSuccess(f) =>
        Logger("CL", Info, "reduced formula:\n  " + FormulaUtils.getConjuncts(f).mkString("\n  "))
        f
      case Typer.TypingFailure(r) =>
        Logger.logAndThrow("CL", Error, "could not type:\n  " + last.map(_.toStringFull).mkString("\n  ") + "\n  " + r)
      case Typer.TypingError(r) =>
        Logger.logAndThrow("CL", Error, "typer failed on:\n  " + last + "\n  " + r)
    }
  }
  
}
