package round.logic

import round.formula._

import dzufferey.utils.Misc
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

//facility to generate additional ground terms (ψ-local theory extensions)

/** define a term generation function
 * @param vars are the free variables in expr that are replaced with ground terms during the generation
 * @param expr is the template expression to generate
 * @param modifiers are a list of function that can additionally be used to check candiates variables
 */
class TermGenerator(_vars: List[Variable],
                    _expr: Formula) {

  val (vars, expr) = Simplify.deBruijnIndex(ForAll(_vars, _expr)) match {
    case ForAll(vs, f) => vs -> f
    case other => Logger.logAndThrow("TermGenerator", Error, "expect ∀, found: " + other)
  }
  
  override def equals(a: Any): Boolean = {
    if (a.isInstanceOf[TermGenerator]) {
      val tg = a.asInstanceOf[TermGenerator]
      tg.vars == vars && tg.expr == expr
    } else false
  }
  override def hashCode: Int = vars.hashCode + expr.hashCode

  //TODO for local version
  //val symbols = FormulaUtils.collectSymbols(expr)
    //TODO for local version
    //val gts2 = gts.view.filter(t => !FormulaUtils.exists({
    //    case Application(s, _) => symbols(s)
    //    case _ => false }, t) ).toVector

  /** returns only the newly generated terms, i.e., the terms not already in gts
   *  TODO this is the (semi) brain-dead version...
   */
  def apply(gts: Set[Formula]): Set[Formula] = {
    val gts2 = gts.toVector.groupBy(_.tpe)
    val candidates = vars.map( v => gts2.getOrElse(v.tpe, Vector.empty) ).toVector
    val tuplified = Misc.cartesianProductIterator(candidates)
    var terms = Set[Formula]()
    while (tuplified.hasNext) {
      val ts = tuplified.next
      val map = vars.view.zip(ts).toMap[Formula,Formula]
      val t = FormulaUtils.map( f => map.getOrElse(f, f), expr)
      if (!gts.contains(t)) {
        terms += t
      }
    }
    terms
  }

}


//Those should be normalized so they can be quickly compared
//applying a term to it should return either nothing, a formula, or a new generator 
//if a new generator is returned the term should also be applied to it (after checking that the generator is not redundant)
//class TermGenPattern(vs: List[Variable], e: Formula) {
//}

//TODO a more efficient version that can be used in InstGen
//CongruenceClosure (normalization and pushing new constraints) -> could be done by the part calling the IncrementalTermGenerator

//TODO Local IncrementalTermGenerator
//with triggers, etc.

//more a formula generator than a term generator
class IncrementalTermGenerator(axioms: Iterable[Formula]) {

  protected def mkGen(vs: Iterable[Variable], f: Formula) = {
    import FormulaUtils._
    new Gen(vs.toArray.sorted, Simplify.simplify(f))
  }

  protected class Gen(val vs: Array[Variable], val f: Formula) {

    override def toString = vs.mkString("Gen( ",", "," → " + f)

    val done = Array.tabulate(vs.size)( _ => scala.collection.mutable.Set[Formula]() )

    def similar(tg: Gen) = {
      tg.vs.size == vs.size &&
      (0 until tg.vs.size).forall(i => tg.vs(i) == vs(i)) &&
      tg.f == f
    }

    def isResult = vs.isEmpty
    def result = {
      assert(isResult)
      f
    }

    def newGen(idx: Int, term: Formula): Gen = {
      val kept = List.tabulate(vs.size -1)( i => if (i < idx) vs(i) else vs(i+1) )
      val subs = FormulaUtils.replace(vs(idx), term, f)
      Simplify.deBruijnIndex(ForAll(kept, subs)) match {
        case ForAll(vs, f) => mkGen(vs,f)
        case other => mkGen(Array.empty[Variable], other)
      }
    }

    def apply(term: Formula): Iterable[Gen] = {
      var i = 0
      var res = List.empty[Gen]
      while(i < vs.size) {
        if (term.tpe == vs(i).tpe && !done(i)(term)) {
          res ::= newGen(i, term)
          done(i) += term
        }
        i += 1
      }
      res
    }

  }

  //the current generators
  import scala.collection.mutable.ArrayBuffer
  protected val idx  = scala.collection.mutable.Map[Type,ArrayBuffer[Int]]()
  protected val gens = ArrayBuffer[Gen]()

  protected def addGen(g: Gen) {
    val potentialConflict = scala.collection.mutable.BitSet()
    g.vs.foreach( v => {
      potentialConflict ++= idx.getOrElseUpdate(v.tpe, ArrayBuffer[Int]())
    })
    if (potentialConflict.forall( i => !gens(i).similar(g))) {
      gens.append(g)
      g.vs.foreach( v => {
        val buffer = idx.getOrElseUpdate(v.tpe, ArrayBuffer[Int]())
        buffer += gens.size -1
      })
    }
  }
  
  //extract the first Gen from the axioms
  axioms.foreach{
    case fa @ ForAll(_, _) =>
      Simplify.deBruijnIndex(fa) match {
        case ForAll(vs, f) => addGen( mkGen(vs, f) )
        case other => Logger("TermGenerator", Warning, "(1) expect ∀, found: " + other)
      }
    case other => Logger("TermGenerator", Warning, "(2) expect ∀, found: " + other)
  }
  
  
  def generate(term: Formula): List[Formula] = {
    val candidate = idx.getOrElseUpdate(term.tpe, ArrayBuffer[Int]())
    var i = 0
    var res = List.empty[Formula]
    while(i < candidate.size) {
      val newGens = gens(candidate(i))(term)
      newGens.foreach( g => {
        if (g.isResult) {
          if (g.result != True()) res ::= g.result
        } else addGen(g)
      })
      i += 1
    }
    res
  }

  def generate(groundTerms: Set[Formula]): List[Formula] = {
    groundTerms.toList.flatMap(generate)
  }

}
  
