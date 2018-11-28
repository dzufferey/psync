package psync.logic

import psync.formula._
import psync.logic.quantifiers._
import psync.utils.Stats

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer

//doing the manual instantiation is expensive and does not scale.
//instead we can just send all the axioms to the solver
//the downside is that if the formula is sat, the solver will most likely never return

class ClAxiomatized(config: ClConfig) {

  //from the config:
  //  `onType` is as expected
  //  vennBound > 0 means add axiom for venn regions of pairs of sets
  //  instantiationStrategy.local means member-card axioms are universal only (incomplete)

  //TODO make sure it also deals with FMap(K, V)
  //TODO do we need to add ∀ X. card(X) >= 0

  import CL._

  protected def getSetElementTypeFromSymbol(s: Symbol): Type = s.tpe match {
    case FSet(t) => t
    case Function(_, FSet(t)) => t
    case t => Logger.logAndThrow("ClAxiomatized", Error, "getSetElementTypeFromSymbol: not a set " + t)
  }

  protected def membershipAxioms(comprehensionDef: Map[Symbol, Formula]): List[Formula] = {
    comprehensionDef.flatMap{ case (sym, axiom) =>
      val t = getSetElementTypeFromSymbol(sym)
      val select = config.onType.map(_ contains t).getOrElse(true)
      if (select) {
        Some(axiom match {
          case ForAll(params, Eq(sym, Comprehension(List(e), body))) =>
            ForAll(e :: params, Eq(In(e, sym), body))
          case Eq(sym, Comprehension(List(e), body)) =>
            ForAll(List(e), Eq(In(e, sym), body))
          case other =>
            Logger.logAndThrow("ClAxiomatized", Error, "membershipAxioms: not the right form " + other)
        })
      } else None
    }.toList
  }

  // universalOnly (only good if at least one element to instantiate member)
  //   ∀ x X. (card(X) = 0 ⇒ x∉X)
  //   ∀ x X. (x∈X ⇒ card(X) > 0)
  // ¬universalOnly adds
  //   ∀ X. ∃ x. card(X) > 0 ⇒ x∈X
  protected def emptinessAxiom(tpe: Type): List[Formula] = {
    val universalOnly = false //config.instantiationStrategy.local
    val e = Variable(Namer("elt")).setType(tpe)
    val x = Variable(Namer("X")).setType(FSet(tpe))
    val univ = ForAll(List(x, e), And(
      Implies(Eq(Cardinality(x), Literal(0)), Not(In(e, x))),
      Implies(In(e, x), Not(Eq(Cardinality(x), Literal(0))))
    ))
    val exis = ForAll(List(x), Exists(List(e),
      Implies(Not(Eq(Cardinality(x), Literal(0))), In(e, x))
    ))
    if (universalOnly) List(univ) else List(univ, exis)
  }

  // ∀ X Y.
  //    |universe| ≥ card(X ∩ Y) + card(X ∩ ¬Y) + card(¬X ∩ Y)
  //    card(X) = card(X ∩ Y) + card(X ∩ ¬Y)
  //    card(Y) = card(X ∩ Y) + card(¬X ∩ Y)
  protected def vennAxioms(tpe: Type): List[Formula] = {
    val ctpe = Function(List(FSet(tpe)), FSet(tpe))
    val complement = UnInterpretedFct("_complement_"+tpe, Some(ctpe), List())
    sizeOfUniverse(tpe) match {
      case Some(u) =>
        val e = Variable(Namer("elt")).setType(tpe)
        val x = Variable(Namer("X")).setType(FSet(tpe))
        val y = Variable(Namer("Y")).setType(FSet(tpe))
        val cx = complement(x)
        val cy = complement(y)
        val c0 = Cardinality(Intersection( x, y)) //TODO communtativity of intersection ?
        val c1 = Cardinality(Intersection( x,cy))
        val c2 = Cardinality(Intersection(cx, y))
        val c3 = Cardinality(Intersection(cx,cy))
        List(
          ForAll(List(x, y), Eq(u, Plus(c0, c1, c2, c3))),
          ForAll(List(x), Eq(u, Plus(Cardinality(x), Cardinality(cx)))),
          ForAll(List(x, y), Eq(Cardinality(x), Plus(c0, c1))),
          ForAll(List(x, y), Eq(Cardinality(y), Plus(c0, c2))),
          ForAll(List(x, e), Eq(In(e, x), Not(In(e, cx)))),
          ForAll(List(x), Eq(x, complement(cx)))
        )
      case None =>
        Nil
    }
  }

  def cardPos(tpe: Type): Formula = {
    val x = Variable(Namer("X")).setType(FSet(tpe))
    ForAll(List(x), Leq(Literal(0), Cardinality(x)))
  }

  def reduce(formula: Formula): Formula = {
    // begin same preprocessing as CL.reduce
    val query = normalize(formula)
    assert(Typer(query).success, "ClAxiomatized.reduce, not well typed")
    //For comprehension body
    var symbols = Map.empty[Symbol,Formula]
    //remove the top level ∃ quantifiers (sat query)
    val (query1, _) = getExistentialPrefix(query)
    val clauses0 = FormulaUtils.getConjuncts(query1)
    val clauses1 = clauses0.map( f => {
      val f2 = Simplify.lazyPnf(f)
      val f3 = fixUniquelyDefinedUniversal(f2)
      FormulaUtils.map({
        case c @ Comprehension(_, _) =>
          val (sym, d, args) = symbolizeComprehension(c)
          symbols += (sym -> d)
          sym(args:_*)
        case other => other
      }, f3)
    })
    val clauses2 = clauses1.map(remove2ndOrderNeq)
    val clauses3 = FormulaUtils.getConjuncts(getExistentialPrefix(normalize(And(clauses2:_*)))._1)
    val clauses = clauses3.map(skolemize)
    // end same preprocessing as CL.reduce
    Logger("ClAxiomatized", Debug, "clauses:\n  " + clauses.mkString("\n  "))

    // axioms for comprehension membership
    val membership = membershipAxioms(symbols)
    Logger("ClAxiomatized", Debug, "membership axioms:\n  " + membership.mkString("\n  "))
    // axioms for venn regions (just based on type and membership, quantify over set)
    val onType = config.onType.getOrElse({
        val terms = FormulaUtils.collectGroundTerms(And(clauses:_*)) ++ FormulaUtils.collectVariables(And(clauses:_*))
        terms.flatMap(_.tpe match {
          case FSet(t) => Some(t)
          case FMap(t, _) => Some(t)
          case _ => None
        })
      }).toList
    val empty = onType.flatMap(emptinessAxiom)
    val pos = onType.map(cardPos)
    Logger("ClAxiomatized", Debug, "emptiness axioms:\n  " + membership.mkString("\n  "))
    val venn = if (config.vennBound.getOrElse(1) > 0) {
        onType.flatMap( t => vennAxioms(t) )
      } else {
        Nil
      }
    Logger("ClAxiomatized", Debug, "venn axioms:\n  " + venn.mkString("\n  "))
    //TODO add n > 0 only if n is in the rest
    val clausesWithVenn = Lt(Literal(0), n) :: clauses ::: pos ::: empty ::: membership ::: venn

    // axioms for extra theory
    val extraAxioms = AxiomatizedTheory.getAxioms(clausesWithVenn)
    Logger("ClAxiomatized", Debug, "extraAxioms:\n  " + extraAxioms.mkString("\n  "))
    val withoutTime = ReduceTime(clausesWithVenn ::: extraAxioms)
    val expendedLt = ReduceOrdered(withoutTime)
    val last = cleanUp(expendedLt)
    assert(Typer(last).success, "ClAxiomatized.reduce, not well typed")
    last
  }

}
