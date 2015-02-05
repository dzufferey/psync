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

  protected def normalize(f: Formula) = {
    //TODO some CNF conversion ?
    //TODO purification before or after instantiation ?
    val f1 = Simplify.normalize(f)
    //println(f1)
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
        acc.flatMap( f => Quantifiers.instantiateWithTerms(v, f, candidates.toSet) )
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
  case class SetDef(scope: Set[Variable], id: Formula, body: Option[Formula]) {
    def tpe = id.tpe
    def contentTpe = tpe match {
      case FSet(t) => t
      case _ => Logger.logAndThrow("CL", Error, "SetDef had not FSet type")
    }
  }

  //TODO assumes positive occurance!!
  protected def namedComprehensions(conjuncts: List[Formula]): (List[Formula], Set[SetDef]) = {
    var acc = Set[SetDef]()
    def process(bound: Set[Variable], f: Formula) = f match {
      case Eq(id, c @ Comprehension(vs, body)) => 
        val scope = bound intersect (body.freeVariables -- vs)
        acc += SetDef(scope, id, Some(c))
        True()
      case Eq(c @ Comprehension(vs, body), id) => 
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
      case c @ Comprehension(vs, body) => 
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

  //TODO generalize to the size of the universe for other types
  protected val procIdCardinalityAxioms = {
    val s = Variable("s").setType(FSet(procType))
    List(
      ForAll(List(s), Leq(Cardinality(s), n)),
      Lt(Literal(0), n)
    )
  }

  //TODO generalize to more than two
  protected def mkPairILP( set1: SetDef, set2: SetDef): List[Formula] = {
    val clashing = set1.scope intersect set2.scope
    def rename(set: SetDef) = {
      val fresh = set.scope.flatMap(v =>
        if (clashing(v)) Some(v -> Variable(Namer(v.name)).setType(v.tpe))
        else None ).toMap
      SetDef( set.scope.map(FormulaUtils.alphaAll(fresh, _).asInstanceOf[Variable]),
              FormulaUtils.alphaAll(fresh, set.id),
              set.body.map(FormulaUtils.alphaAll(fresh, _)) )
    }
    val rset1 = rename(set1)
    val rset2 = rename(set2)
    val tpe = rset1.tpe
    val ctpe = rset1.contentTpe
    assert(rset2.tpe == tpe, "set of different type")
    val params = rset1.scope ++ rset2.scope
    def mkSkolem(name: String, tpe: Type) = {
      Quantifiers.skolemify(Variable(Namer(name)).setType(tpe), params)
    }
    val tt = mkSkolem("venn_tt", Int)
    val tf = mkSkolem("venn_tf", Int)
    val ft = mkSkolem("venn_ft", Int)
    val ff = mkSkolem("venn_ff", Int)
    val vtt = mkSkolem("sk_venn_tt", ctpe)
    val vtf = mkSkolem("sk_venn_tf", ctpe)
    val vft = mkSkolem("sk_venn_ft", ctpe)
    val vff = mkSkolem("sk_venn_ff", ctpe)
    val posCard = List(
      Leq(Literal(0), tt),
      Leq(Literal(0), tf),
      Leq(Literal(0), ft),
      Leq(Literal(0), ff)
    )
    val universe =
      if(ctpe == procType) List( Eq(n, Plus(Plus(tt,tf),Plus(ft,ff))))
      else {
        Logger("CL", Warning, "do not know the size of the universe of " + ctpe)
        Nil
      }
    val argSets = List(
      Eq(Cardinality(rset1.id), Plus(tt,tf)),
      Eq(Cardinality(rset2.id), Plus(tt,ft))
    )
    val conjuncts = posCard ::: universe ::: argSets
    def unify(v: Formula, com: Formula) = com match {
      case Comprehension(List(i), f) =>
        FormulaUtils.replace(i, v, f)
      case other =>
        sys.error("expected comprehension, found: " + other)
    }
    val triggers = (rset1.body, rset2.body) match {
      case (Some(d1), Some(d2)) =>
        List(
          Implies(Lt(Literal(0), tt), And(unify(vtt,d1),unify(vtt,d2))),
          Implies(Lt(Literal(0), tf), And(unify(vtf,d1),Not(unify(vtf,d2)))),
          Implies(Lt(Literal(0), ft), And(Not(unify(vft,d1)),unify(vft,d2)))
        )
      case (Some(d1), None) =>
        List(
          Implies(Lt(Literal(0), Plus(tt,tf)), unify(vtf, d1))
        )
      case (None, Some(d2)) =>
        List(
          Implies(Lt(Literal(0), Plus(tt,ft)), unify(vft, d2))
        )
      case (None, None) => 
        Nil
    }
    val res = FormulaUtils.getConjuncts(ForAll(params.toList, And((conjuncts ::: triggers): _*)))
  //Logger("CL", Warning, "params("+id1+"):  " + bound1.map(_.toStringFull).mkString(", "))
  //Logger("CL", Warning, "params("+id2+"):  " + bound2.map(_.toStringFull).mkString(", "))
  //Logger("CL", Warning, "mkPairILP:\n  " + res.map(_.toStringFull).mkString("\n  "))
    assert(Typer(And(res:_*)).success)
    res
  }

  /** from A={i. P(i)} to ∀ i. P(i) ⇔ i∈A 
   *  if it is a set of processID also add
   *    |A|=n ⇒ (∀i. P(i))
   *    |A|=0 ⇒ (∀i. ¬P(i))
   *  TODO generalize to other types and size of the universe
   */
  protected def membershipAxioms(compDef: SetDef): List[Formula] = {
    compDef.body match {
      case Some(c @ Comprehension(List(i), f)) =>
        val bound = compDef.scope
        assert(!bound(i))
        val name = compDef.id
        val member = ForAll(i :: bound.toList, Eq(f, In(i, name)))
        if (i.tpe == procType) {
          List(
            ForAll(bound.toList, Implies(Eq(Cardinality(name), n), ForAll(List(i), f))),
            ForAll(bound.toList, Implies(Eq(Cardinality(name), Literal(0)), ForAll(List(i), Not(f)))),
            member
          )
        } else {
          List(member)
        }
      case _ =>
        sys.error("expected comprehension, found " + compDef.body)
    }
  }

  protected def reduceComprehension(conjuncts: List[Formula]): List[Formula] = {
    val (woComp, c1) = collectComprehensionDefinitions(conjuncts)
    val v = Variable("v").setType(procType)
    val ho = SetDef(Set(v), Application(HO, List(v)), None)
    val c2 = if (woComp exists hasHO) c1 + ho else c1
    Logger("CL", Debug, "reduceComprehension, comprehensions:\n  " + c1.mkString("\n  "))
    val ilp = for (s1 <- c2; s2 <- c2 if s1 != s2 &&
                                         s1.tpe == FSet(procType) &&
                                         s2.tpe == FSet(procType))
              yield mkPairILP(s1, s2)
    val membership = c1.toList flatMap membershipAxioms
    woComp ::: procIdCardinalityAxioms ::: membership ::: ilp.toList.flatten
  }

  //∀ e,S. e∈S ⇒ |S|>0
  protected def inAxiom(tpe: Type) = {
    val e = Variable("e").setType(tpe)
    val s = Variable("S").setType(FSet(tpe))
    ForAll(List(e,s), Implies(In(e,s), Gt(Cardinality(s), Literal(0))))
  }

  //∀ S,T,U. U = S∪T ⇒ |U| ≥ |S| ∧ |U| ≥ |T| 
  protected def unionAxiom(tpe: Type) = {
    val s = Variable("S1").setType(FSet(tpe))
    val t = Variable("S2").setType(FSet(tpe))
    val u = Variable("S3").setType(FSet(tpe))
    ForAll(List(s,t,u),
        Implies(Eq(u, Union(s,t)),
                And(Geq(Cardinality(u),Cardinality(s)),
                    Geq(Cardinality(u),Cardinality(t)))))
  }
  
  //∀ S,T,U. U = S∩T ⇒ |U| ≤ |S| ∧ |U| ≤ |T| 
  protected def intersectionAxiom(tpe: Type) = {
    val s = Variable("S1").setType(FSet(tpe))
    val t = Variable("S2").setType(FSet(tpe))
    val u = Variable("S3").setType(FSet(tpe))
    ForAll(List(s,t,u),
        Implies(Eq(u, Intersection(s,t)),
                And(Leq(Cardinality(u),Cardinality(s)),
                    Leq(Cardinality(u),Cardinality(t)))))
  }

  //∀ S,T. S⊆T ⇒ |S| ≤ |T|
  protected def subsetAxiom(tpe: Type) = {
    val s = Variable("S1").setType(FSet(tpe))
    val t = Variable("S2").setType(FSet(tpe))
    ForAll(List(s,t),
        Implies(SubsetEq(s, t),
                Leq(Cardinality(s),Cardinality(t))))
  }

  /* add axioms for set operations */ 
  protected def addSetAxioms(conjuncts: List[Formula]): List[Formula] = {
    val f = And(conjuncts:_*)
    val setOps = FormulaUtils.collectSymbolsWithParams(f).collect{
        case p @ (Union | Intersection | SubsetEq | SupersetEq | In | Contains, _) => p
      }
    val setAxioms = setOps.toList.flatMap{ case (sym, params) =>
      sym match {
        case Union => 
          assert(params.size == 1)
          List(unionAxiom(params.head))
        case Intersection => 
          assert(params.size == 1)
          List(intersectionAxiom(params.head))
        case SubsetEq =>
          assert(params.size == 1)
          List(subsetAxiom(params.head))
        case In =>
          assert(params.size == 1)
          List(inAxiom(params.head))
        case _ =>
          Logger("CL", Warning, "TODO addSetAxioms for " + (sym,params));
          Nil
      }
    }

    //TODO this is ILP with single elements
    val tps = FormulaUtils.collectTypes(f).collect{ case f: FSet => f }.toList
    conjuncts ::: setAxioms ::: tps.flatMap( t => {
      val s = Variable("s").setType(t)
      val pos = ForAll(List(s), Leq(Literal(0), Cardinality(s)))
      if (t == FSet(procType)) {
        List(
          pos,
          ForAll(List(s), Leq(Cardinality(s), n))
        )
      } else {
        List(pos)
      }
    })
  }

  //TODO add axioms for inclusion and card, ....
  
  def reduce(formula: Formula): Formula = {
    val typed = Typer(formula).get 
    val n1 = normalize(typed)
    val n2 = Quantifiers.getExistentialPrefix(n1)._1
    val rawConjuncts = FormulaUtils.getConjuncts(n2)
    val conjuncts = rawConjuncts.map(f => Quantifiers.skolemize(Simplify.simplify(Simplify.pnf(f))))
    Logger("CL", Info, "reducing:\n  " + conjuncts.mkString("\n  "))
    val withILP = reduceComprehension(conjuncts)
    Logger("CL", Debug, "with ILP:\n  " + withILP.mkString("\n  "))
    val withSetAx = addSetAxioms(withILP)
    val withOpt = OptionAxioms.addAxioms(withSetAx)
    val withTpl = TupleAxioms.addAxioms(withOpt)
    //val withTpl = TupleAxioms.addAxioms(withOpt)
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
