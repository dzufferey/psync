package psync.logic

import psync.formula._
import psync.logic.quantifiers._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer

object VennRegions {

  //since we generate fresh variable names we need an eager generator
  protected def mkUniv(generator: EagerGenerator)(f: Formula) = {
    val newClauses = generator.generate(f)
    //println(newClauses.mkString("newClauses\n    ","\n    ",""))
    //val filtered = newClauses.filter(f => !CL.hasComp(f))
    val filtered = newClauses.filter(CL.keepAsIt)
    //println(filtered.mkString("filtered\n    ","\n    ",""))
    And(filtered:_*)
  }
  
  /** Generate the ILP for the given sets.
   * @param tpe the type of the elements in the sets, e.g., ProcessID
   * @param universeSize the size of the universe (if the universe is finite), e.g., 'n' for ProcessID
   * @param sets the sets as pair (id, definition), where the definition is an optional Comprehension.
   * @param generator an incremental generator to instantiate axioms on the new terms
   */
  def apply(tpe: Type,
            universeSize: Option[Formula],
            sets: Iterable[(Formula, Option[Binding])],
            generator: Generator) = {
    new VennRegions(tpe, universeSize, sets, mkUniv(generator.toEager)).constraints
  }

  /** Generate the ILP for the given sets.
   * @param tpe the type of the elements in the sets, e.g., ProcessID
   * @param universeSize the size of the universe (if the universe is finite), e.g., 'n' for ProcessID
   * @param sets the sets as pair (id, definition), where the definition is an optional Comprehension.
   * @param cc (optional) congruence classes of ground terms in the original formula
   * @param univ (optional) the set of universally quantified clauses in the original formula
   */
  def apply(tpe: Type,
            universeSize: Option[Formula],
            sets: Iterable[(Formula, Option[Binding])],
            cc: CC = CongruenceClasses.empty,
            univ: List[Formula] = Nil) = {
    def mkUniv(f: Formula) = InstGen.saturateWith(And(univ:_*), Set(f), Some(1), cc) //TODO try to avoid saturateWith, expensive when there are many terms
    new VennRegions(tpe, universeSize, sets, mkUniv).constraints
  }
  
  /** Generate the ILP for the given sets considering only the intersection of at most bound sets.
   * @param bound the maximal number of sets to consider at once for the Venn Regions
   * @param tpe the type of the elements in the sets, e.g., ProcessID
   * @param universeSize the size of the universe (if the universe is finite), e.g., 'n' for ProcessID
   * @param sets the sets as pair (id, definition), where the definition is an optional Comprehension.
   * @param generator an incremental generator to instantiate axioms on the new terms
   * @param preserveGenerator set to true if you plan to use the generator afterward
   */
  def withBound(bound: Int,
                tpe: Type,
                universeSize: Option[Formula],
                sets: Iterable[(Formula, Option[Binding])],
                generator: Generator,
                preserveGenerator: Boolean): Formula = {
    val fct: Formula => Formula =
      if (preserveGenerator) {
        mkUniv(generator.toEager)
      } else {
        val elt = Variable(Namer("elt")).setType(tpe)
        val clauses = mkUniv(generator.toEager)(elt)
        ( (f: Formula) => FormulaUtils.replace(elt, f, clauses) )
      }
    new VennRegionsWithBound(bound, tpe, universeSize, sets, fct).constraints
  }
  
  /** Generate the ILP for the given sets considering only the intersection of at most bound sets.
   * @param bound the maximal number of sets to consider at once for the Venn Regions
   * @param tpe the type of the elements in the sets, e.g., ProcessID
   * @param universeSize the size of the universe (if the universe is finite), e.g., 'n' for ProcessID
   * @param sets the sets as pair (id, definition), where the definition is an optional Comprehension.
   * @param generator an incremental generator to instantiate axioms on the new terms
   */
  def withBound(bound: Int,
                tpe: Type,
                universeSize: Option[Formula],
                sets: Iterable[(Formula, Option[Binding])],
                generator: Generator): Formula = {
    withBound(bound, tpe, universeSize, sets, generator, true)
  }

  /** Generate the ILP for the given sets considering only the intersection of at most bound sets.
   * @param bound the maximal number of sets to consider at once for the Venn Regions
   * @param tpe the type of the elements in the sets, e.g., ProcessID
   * @param universeSize the size of the universe (if the universe is finite), e.g., 'n' for ProcessID
   * @param sets the sets as pair (id, definition), where the definition is an optional Comprehension.
   * @param cc (optional) congruence classes of ground terms in the original formula
   * @param univ (optional) the set of universally quantified clauses in the original formula
   */
  def withBound(bound: Int,
                tpe: Type,
                universeSize: Option[Formula],
                sets: Iterable[(Formula, Option[Binding])],
                cc: CC = CongruenceClasses.empty,
                univ: List[Formula] = Nil): Formula  = {
    val cc2 = cc.copy //avoids feeding new terms (because it will be replace)
    val template = Variable(Namer("__template")).setType(tpe)
    val generated = InstGen.saturateWith(And(univ:_*), Set(template), Some(1), cc2) //TODO even better this could be shared across sets of different types
    def mkUniv(f: Formula) = FormulaUtils.replace(template, f, generated)
    new VennRegionsWithBound(bound, tpe, universeSize, sets, mkUniv).constraints
  }

}

/** A class to link the cardinality constraints over sets to the elements in the sets.
 * @param tpe the type of the elements in the sets, e.g., ProcessID
 * @param universeSize the size of the universe (if the universe is finite), e.g., 'n' for ProcessID
 * @param sets the sets as pair (id, definition), where the definition is an optional Comprehension.
 * @param mkUniv a function that takes some generated variable and returns constraints (typically instanciate some ∀ over that element)
 */
class VennRegions(tpe: Type,
                  universeSize: Option[Formula],
                  sets: Iterable[(Formula, Option[Binding])],
                  mkUniv: Formula => Formula) {

  /** Removes funny characters from string to make them smt-lib compliant. */
  protected def sanitize(str: String) = {
    str.replace('(','_').replace(')','_').replaceAll("\\*","").replaceAll("->","-")
  }

  /** the prefix for the name of the integer variables representing the size of the Venn regions. */
  protected val prefix = Namer("venn_" + sanitize(tpe.toString)) + "_"
  
  protected val elt = Variable(Namer("elt")).setType(tpe)
  protected val univCstr = mkUniv(elt)

  protected var counter = 0

  /** Set name to index */
  protected var idToPos = Map[Formula, Int]()

  /** Index to set name */
  protected var posToId = Map[Int, Formula]()

  protected var nbrVennRegions = 1

  // Initialization
  sets.foreach{ case (id, definition) =>
    assert(id.tpe == FSet(tpe), "set has the wrong type: " + id.tpe + " instead of " + FSet(tpe))
    definition match {
      case Some(Binding(Comprehension, List(_), _)) | None => ()
      case other => Logger.logAndThrow("VennRegions", Error, "definition should be a Comprehension, found: " + other)
    }
    if (!idToPos.contains(id)) {
      id match {
        case Not(_) =>
          Logger.logAndThrow("VennRegions", Error, "'add' only takes identifiers in the positive form: " + id)
        case _ =>
          idToPos += (id -> counter)
          posToId += (counter -> id)
          counter += 1
          nbrVennRegions *= 2
      }
    }
  }
  assert(counter < 32, "will run into indexing problems counter = "+counter+" (tpe = "+tpe+")\n" ++ sets.mkString("\n"))
  
  /** Generate all the constraints:
   * a: |p| + |~p| = |universe|
   * b: ∀ i. i ∈ p ⇒ |p| ≥ 1
   * c: |p| ≥ 1 ⇒ ∃ i. i ∈ p
   * d: |p| ≥ 0
   * e: ∀ i. i ∈ p ⇔ p(i)
   * f: |p| = Σ ...
   */
  def constraints: Formula = {
    val a = sumToUniverse 
    val b = nonEmptyNonZeroCard
    val c = nonZeroCardNonEmpty
    val d = positiveVariables
    val e = membershipAxioms
    val f = topLevelLink
    val cstrs = And(((a +: b) ++ c ++ d ++ e ++ f):_*)
    val o1: Set[Variable] = universeSize.map(_.freeVariables).getOrElse(Set())
    val o2 = sets.flatMap{ case (f, b) => f.freeVariables ++ b.map(_.freeVariables).getOrElse(Set()) }
  //scope of variables generated by the venn regions 
    val outside: Set[Variable] = o1 ++ o2
    val scope = cstrs.freeVariables -- outside
    Exists(scope.toList, cstrs)
  }

  /** `get(id)` and `get(Not(id))` for the complement
   * @param id the name of a set
   */
  protected def idx(id: Formula): Int = id match {
    case Not(Not(id2)) => idx(id2)
    case Not(id2) => idx(id2)
    case _ => idToPos(id)
  }

  /** Test whether the given formula is negated or not*/
  protected def polarity(id: Formula): Boolean = id match {
    case Not(Not(id2)) => polarity(id2)
    case Not(id2) => false
    case _ => true
  }

  //the sum of the regions where id holds
  def getCardDef(id: Formula): Formula = {
    val index = prefix.length + idx(id)
    val pol = if(polarity(id)) 't' else 'f'
    val vs = ennumerate.filter( v => v.name(index) == pol )
    Application(Plus, vs.toList).setType(Int)
  }

  //a: |p| + |~p| = |universe|
  def sumToUniverse: Formula = universeSize match {
    case Some(s) =>
      if (counter == 0) Eq(s, ennumerate.next)
      else Eq(s, Plus(ennumerate.toList:_*).setType(Int))
    case None => True()
  }

  //b: ∀ i. i ∈ p ⇒ |p| ≥ 1
  def nonEmptyNonZeroCard: Seq[Formula] = {
    for(i <- 0 until nbrVennRegions) yield {
      ForAll(List(elt), Implies(mkMembership(elt, i), Leq(Literal(1), mkVar(i))))
    }
  }

  //c: |p| ≥ 1 ⇒ ∃ i. i ∈ p
  def nonZeroCardNonEmpty: Seq[Formula] = {
    for(i <- 0 until nbrVennRegions) yield {
      Exists(List(elt), Implies(Leq(Literal(1), mkVar(i)), And(mkMembership(elt, i), univCstr)))
    }
  }

  //d: |p| ≥ 0
  def positiveVariables: Seq[Formula] = {
    val pos = ennumerate.map( v => Leq(Literal(0), v) )
    pos.toSeq
  }

  //e: ∀ i. i ∈ p ⇔ p(i)
  def membershipAxioms: Seq[Formula] = {
    (for( (id, comp) <- sets;
         binding <- comp ) yield binding match {
      case Comprehension(List(v), body) =>
        ForAll(List(v), Eq(In(v, id), body))
      case other =>
        Logger.logAndThrow("VennRegions", Error, "expected Comprehension, found: " + other)
    }).toSeq
  }


  //f: |p| = Σ ...
  def topLevelLink: Seq[Formula] = {
    sets.map{ case (id, _) =>
      Eq(Cardinality(id), getCardDef(id))
    }.toSeq
  }

  protected val builder = new StringBuilder(prefix.size + sets.size)
  protected def mkVar(n: Int) = {
    builder.append(prefix)
    var acc = n
    var i = 0
    while (i < counter) {
      if (acc % 2 == 0) {
        builder.append('t')
      } else {
        builder.append('f')
      }
      acc /= 2
      i += 1
    }
    val v = Variable(builder.toString).setType(Int)
    builder.clear
    v
  }

  protected def mkMembership(elt: Formula, n: Int) = {
    var acc = n
    var ms: List[Formula] = Nil
    var i = 0
    while (i < counter) {
      val mem = In(elt, posToId(i))
      ms ::= (if (acc % 2 == 0) mem else Not(mem))
      acc /= 2
      i += 1
    }
    And(ms:_*)
  }

  //list all the variables in the environement
  def ennumerate: Iterator[Variable] = {
    new Iterator[Variable] {
      private var c = 0

      def hasNext = c < nbrVennRegions

      def next = {
        val v = mkVar(c)
        c += 1
        v
      }
    }
  }

}

/** Same as VennRegions but considers only the intersection of at most bound sets.
 * @param bound the maximal number of sets to consider at once for the Venn Regions
 */
class VennRegionsWithBound(bound: Int,
                           tpe: Type,
                           universeSize: Option[Formula],
                           _sets: Iterable[(Formula, Option[Binding])],
                           mkUniv: Formula => Formula) {

  protected val sets = _sets.toArray

  protected def choose(n: Int, k: Int): Long = {
    var c = 1l
    for (i <- (math.max(k, n-k) + 1) to n) c *= i
    for (i <- 1 to math.min(k, n-k)) c /= i
    c
  }

  protected def costWithBound: Long = {
    val regions = math.pow(2, math.min(sets.length,bound)).toLong
    val combinations = choose(sets.length, bound)
    regions * combinations
  }
  protected def costFull: Long = math.pow(2, sets.length).toLong

  protected def mkSeq(idx: Int, pos: Int): Seq[List[(Formula, Option[Binding])]] = {
    if (idx >= bound) {
      Seq(Nil)
    } else {
      for (i <- pos to sets.size - bound + idx;
           lst <- mkSeq(idx + 1, i+1)) yield sets(i) :: lst

    }
  }

  def constraints = {
    val cb = costWithBound
    val cf = costFull
    if (cb >= cf || bound >= sets.size) {
      Logger("VennRegions", Info, "doing the full construction because it is cheaper: " + cf + " instead of " + cb + " regions.")
      val vr = new VennRegions(tpe, universeSize, sets, mkUniv)
      vr.constraints
    } else {
      val seq = mkSeq(0,0)
      val cstrs = seq/*.par*/.map( s => new VennRegions(tpe, universeSize, s, mkUniv).constraints )/*.seq*/
      And(cstrs:_*)
    }
  }

}

