package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer

/** A class to link the cardinality constraints over sets to the elements in the sets.
 * @param tpe the type of the elements in the sets, e.g., ProcessID
 * @param universeSize the size of the universe (if the universe is finite), e.g., 'n' for ProcessID
 * @param sets the sets as pair (id, definition), where the definition is an optional Comprehension.
 */
class VennRegions(tpe: Type, universeSize: Option[Formula], sets: Iterable[(Formula, Option[Binding])]) {

  /** Removes funny characters from string to make them smt-lib compliant. */
  protected def sanitize(str: String) = {
    str.replace('(','_').replace(')','_').replaceAll("\\*","").replaceAll("->","-")
  }

  /** the prefix for the name of the integer variables representing the size of the Venn regions. */
  protected val prefix = Namer("venn_" + sanitize(tpe.toString)) + "_"
  
  protected val elt = Variable(Namer("elt")).setType(tpe)

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

  /** Returns the variable associated with the given set names */
  def getVennRegion(ids: Iterable[Formula]): Variable = {
    val map = ids.map( id => idx(id) -> (if (polarity(id)) "t" else "f") ).toMap
    val suffix = for (i <- 0 until counter) yield map.getOrElse(i, "_")
    Variable(prefix + suffix.mkString("")).setType(Int)
  }

  def getVennRegion(id: Formula): Variable = getVennRegion(List(id))


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
      Exists(List(elt), Implies(Leq(Literal(1), mkVar(i)), mkMembership(elt, i)))
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


class VennRegionsWithBound(bound: Int, tpe: Type, universeSize: Option[Formula], _sets: Iterable[(Formula, Option[Binding])]) {

  protected def sets = _sets.toArray

  protected def mkSeq(idx: Int, pos: Int): Seq[List[(Formula, Option[Binding])]] = {
    if (idx >= bound) {
      Seq(Nil)
    } else {
      for (i <- pos to sets.size - bound + idx;
           lst <- mkSeq(idx + 1, i+1)) yield sets(i) :: lst

    }
  }

  def constraints = {
    val seq = if (bound >= sets.size) Seq(_sets)
              else mkSeq(0,0)
    val cstrs = seq.map( s => new VennRegions(tpe, universeSize, s).constraints )
    //TODO there are redundant constraints
    And(cstrs:_*)
  }

}
