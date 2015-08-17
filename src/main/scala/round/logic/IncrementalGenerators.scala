package round.logic

import round.formula._

import dzufferey.utils.{Misc, Namer}
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._


class IncrementalFormulaGenerator(axioms: Iterable[Formula]) extends Cloneable {

  //the current generators
  import scala.collection.mutable.ArrayBuffer
  protected val idx  = scala.collection.mutable.Map[Type,ArrayBuffer[Int]]()
  protected val gens = ArrayBuffer[Gen]()
  
  //speed-up the findSimilar test by keeping the hashes of existing generators
  protected val hashFilter = scala.collection.mutable.Map[Int,ArrayBuffer[Int]]()

  protected def findSimilar(g: Gen): Option[Int] = {
    val potentialConflict = hashFilter.getOrElseUpdate(g.hashCode, ArrayBuffer[Int]())
    potentialConflict.find( i => gens(i).similar(g))
  }

  protected def addGen(g: Gen) {
    if (findSimilar(g).isEmpty) {
      gens.append(g)
      val index = gens.size - 1
      val buffer = hashFilter.getOrElseUpdate(g.hashCode, ArrayBuffer[Int]())
      buffer += index
      g.vs.foreach( v => {
        val buffer = idx.getOrElseUpdate(v.tpe, ArrayBuffer[Int]())
        buffer += index
      })
      if (gens.size % 1000 == 0) {
        Logger("IncrementalGenerator", Debug, "#generator: " + gens.size)
      }
    }
  }
  
  //extract the first Gen from the axioms
  axioms.foreach{
    case fa @ ForAll(vs, f) => addGen( Gen(vs, f) )
    case other => Logger("IncrementalGenerator", Warning, "(2) expect ∀, found: " + other)
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
  
  def generate(groundTerms: Iterable[Formula]): List[Formula] = {
    groundTerms.toList.flatMap(generate)
  }

  def locallySaturate(cc: CC): List[Formula] = {
    var i = 0
    val done = scala.collection.mutable.BitSet()
    val res = scala.collection.mutable.ListBuffer[Formula]()
    def checkDone(g: Gen, remaining: Iterable[Map[Variable,Formula]]) = {
      if (g.isResult) {
        assert(remaining.isEmpty || remaining.forall(_.isEmpty))
        if (g.result != True()) res += g.result
        true
      } else {
        false
      }
    }
    def instVar(g: Gen, matches: Iterable[Map[Variable,Formula]]) {
      if (!checkDone(g, matches)) {
        val v = g.vs.last
        //println(matches.mkString(g + "\n  ", "\n  ", ""))
        val byV = matches.filter(_ contains v).groupBy(_(v)) //matches without v are term generating due to existential quantifiers
        for ( (candidate, maps) <- byV;
              g2 <- g(v, candidate) ) {
          val renaming = g.vs.zip(g2.vs).toMap //g2 has renamed arguments ...
          val remaining = maps.map( m => {
            val m1 = m - v 
            m1.map{ case (a,b) => renaming(a) -> b }
          })
          complete(g2, remaining)
        }
      }
    }
    def complete(g: Gen, remaining: Iterable[Map[Variable,Formula]]) {
      if (!checkDone(g, remaining)) findSimilar(g) match {
        case Some(i) =>
          val g2 = gens(i)
          done += i
          instVar(g2, remaining)
        case None =>
          done += gens.size
          addGen(g)
          instVar(g, remaining)
      }
    }
    while(i < gens.size && !done(i)) {
      val g = gens(i)
      instVar(g, g.localMatches(cc))
      i += 1
    }
    res.result
  }

  override def clone() = {
    val g = new IncrementalFormulaGenerator(axioms)
    g.idx.clear
    idx.foreach{ case (k,v) => g.idx += (k -> v.clone) }
    g.gens.clear
    gens.foreach( gen => g.gens.append(gen.clone) )
    g.hashFilter.clear
    hashFilter.foreach{ case (k,v) =>
      g.hashFilter += (k -> v.clone)
    }
    g
  }

}


class IncrementalGenerator(f: Formula, val cc: CongruenceClosure = new CongruenceClosure) extends Cloneable {

  //make sure the current equalities are in the cc
  cc.addConstraints(f)

  var leftOver: List[Formula] = Nil
  protected var gen = {
    val (axioms, other) = FormulaUtils.getConjuncts(f).partition(Quantifiers.hasFAnotInComp)
    leftOver = other
    new IncrementalFormulaGenerator(axioms.map(Simplify.pnf))
  }

  def generate(term: Formula): List[Formula] = {
    val r = cc.repr(term)
    val newInst = gen.generate(r)
    cc.addConstraints(newInst)
    newInst
  }

  def generate(terms: Set[Formula]): List[Formula] = {
    val buffer = scala.collection.mutable.ListBuffer[Formula]()
    terms.foreach(t => buffer.appendAll(generate(t)))
    buffer.result
  }

  def generateWithExistingGTS = generate(cc.groundTerms)

  /** saturate starting with the groundTerms (representative in cc), up to a certain depth.
   * @param depth (optional) bound on the recursion depth
   * @return applications of the axioms
   */
  def saturate(depth: Option[Int] = None) = {
    val buffer = scala.collection.mutable.ListBuffer[Formula]()
    var d = depth
    var processed = scala.collection.mutable.Set[Formula]()
    var toProcess = cc.groundTerms.map(cc.repr)
    while (d.getOrElse(1) > 0 && !toProcess.isEmpty) {
      Logger("IncrementalGenerator", Debug, "saturate with |toProcess| = " + toProcess.size + ", depth = " + d)
      val newInst = generate(toProcess)
      buffer ++= newInst
      processed ++= toProcess
      val newGts = newInst.view.flatMap(FormulaUtils.collectGroundTerms)
      toProcess = newGts.map(cc.repr).filter(f => !processed.contains(f)).toSet
      d = d.map(_ - 1)
    }
    Logger("IncrementalGenerator", Debug, "saturate before local: " + buffer.size + " new clauses")
    if (d.getOrElse(1) == 0) {
      buffer ++= gen.locallySaturate(cc)
    }
    Logger("IncrementalGenerator", Debug, "saturate generated " + buffer.size + " new clauses")
    buffer.result
  }

  override def clone = {
    val ig = new IncrementalGenerator(f, cc.copy)
    ig.gen = gen.clone
    ig
  }

}


protected class Gen(val vs: Array[Variable], val f: Formula) extends Cloneable {

  override def toString = vs.mkString("Gen( ",", "," → " + f)

  override def hashCode = f.hashCode

  override def clone = {
    val g = new Gen(vs, f)
    var i = 0
    while (i < vs.size) {
      g.done(i) = done(i).clone
      i += 1
    }
    g
  }

  val done = Array.tabulate(vs.size)( _ => scala.collection.mutable.Set[Formula]() )

  def similar(tg: Gen) = {
    tg.vs.size == vs.size &&
    tg.f == f &&
    (0 until tg.vs.size).forall(i => tg.vs(i) == vs(i))
  }

  def isResult = vs.isEmpty
  def result = {
    assert(isResult)
    f
  }

  def newGen(idx: Int, term: Formula): Gen = {
    val kept = List.tabulate(vs.size -1)( i => if (i < idx) vs(i) else vs(i+1) )
    val subs = FormulaUtils.replace(vs(idx), term, f)
    Gen(kept, subs)
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
  
  def apply(v: Variable, term: Formula): Option[Gen] = {
    val i = vs.indexOf(v)
    if (!done(i)(term)) {
      done(i) += term
      Some(newGen(i, term))
    } else {
      None
    }
  }

  def localMatches(cc: CC): Set[Map[Variable,Formula]] = {
    val toIdx = vs.zipWithIndex.toMap
    def notDone(m: Map[Variable,Formula]): Boolean = {
      m.forall{ case (v,f) => !done(toIdx(v))(f) }
    }
    Matching.findLocalSubterms(cc, vs.toSet, f).filter(notDone)
  }

}

object Gen {
  
  protected def mkVar(v: Variable) = {
    Variable(Namer("_generatedExistential")).setType(v.tpe)
  }

  //check ∃∀: if the top level is ∃, then give it a fresh name and continue
  def apply(vs: List[Variable], f: Formula): Gen = {
    if (vs.isEmpty) {
      f match {
        case Exists(vse, fe) =>
          val renaming = vse.map( v => (v -> mkVar(v) ) ).toMap
          FormulaUtils.alpha(renaming, fe) match {
            case ForAll(va, fa) => apply(va, fa)
            case other => apply(Nil, other)
          }
        case other =>
          new Gen(Array.empty[Variable], other)
      }
    } else {
      Simplify.deBruijnIndex(ForAll(vs, f)) match {
        case ForAll(va, fa) =>
          import FormulaUtils._
          new Gen(va.toArray.sorted, Simplify.simplify(fa))
        case other => apply(Nil, other)
      }
    }
  }

}
