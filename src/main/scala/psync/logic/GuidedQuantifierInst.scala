package psync.logic

import psync.formula._

import dzufferey.utils.{Misc, Namer}
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.ArrayBuffer

//FIXME this is a quick and dirty verion
//it needs to be refactored. much code can be reused between this and the IncrementalGenerator

class GuidedQuantifierInst(axioms: Iterable[Formula], cc: CongruenceClosure) {
  
  //the current generators
  protected val idx  = scala.collection.mutable.Map[Type,ArrayBuffer[Int]]()
  protected val gens = ArrayBuffer[GGen]()
  
  //speed-up the findSimilar test by keeping the hashes of existing generators
  protected val hashFilter = scala.collection.mutable.Map[Int,ArrayBuffer[Int]]()

  protected def findSimilar(g: GGen): Option[Int] = {
    val potentialConflict = hashFilter.getOrElseUpdate(g.hashCode, ArrayBuffer[Int]())
    potentialConflict.find( i => gens(i).similar(g))
  }
  
  protected def addGen(g: GGen) {
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
    case fa @ ForAll(vs, f) => addGen( GGen(vs, f, cc) )
    case other => Logger("IncrementalGenerator", Warning, "(2) expect ∀, found: " + other)
  }
  

  def generate(term: Formula): List[Formula] = {
    val candidate = idx.getOrElseUpdate(term.tpe, ArrayBuffer[Int]())
    var i = 0
    val res = MSet[Formula]()//List.empty[Formula]
    val newGens = scala.collection.mutable.Stack[GGen]()
    while(i < candidate.size) {
      val c = candidate(i)
      val s = gens(c).vs.size
      newGens.pushAll( gens(c)(term, cc) ) //this could be cut short by directly giving a ref to the stack
      while(!newGens.isEmpty) {
        val g = newGens.pop
        if (g.isResult) {
          if (g.result != True()) res += g.result
        } else {
          addGen(g)
        }
      }
      i += 1
    }
    res.toList
  }
  
  def generate(groundTerms: Iterable[Formula]): List[Formula] = {
    groundTerms.flatMap(generate).toList
  }
  
  def locallySaturate: List[Formula] = {
    ???
  }
  
  def clone(cc2: CongruenceClosure) = {
    ???
  }

  def log(lvl: Level) {
    Logger("GuidedQuantifierInst", lvl, {
      "TODO"
    })
  }

}

//TODO share the same interface as IncrementalGenerator
class GuidedGenerator(f: Formula, val cc: CongruenceClosure = new CongruenceClosure) extends Cloneable {
  
  //make sure the current equalities are in the cc
  cc.addConstraints(f)

  var leftOver: List[Formula] = Nil
  protected var gen = {
    val (axioms, other) = FormulaUtils.getConjuncts(f).partition(Quantifiers.hasFAnotInComp)
    leftOver = other
    new GuidedQuantifierInst(axioms.map(Simplify.pnf), cc)
  }

  def generate(term: Formula): List[Formula] = {
    //TODO currently avoiding the cc repr due to the way generated terms are checked
    val newInst = gen.generate(term)
    cc.addConstraints(newInst)
    newInst
  }

  def generate(terms: Set[Formula]): List[Formula] = {
    val buffer = scala.collection.mutable.ListBuffer[Formula]()
    //TODO currently avoiding the cc repr due to the way generated terms are checked
    terms.foreach(t => buffer.appendAll(gen.generate(t)))
    buffer.foreach( cc.addConstraints )
    buffer.result
  }

  def generateWithExistingGTS = generate(cc.groundTerms)

  /** saturate starting with the groundTerms (representative in cc), up to a certain depth.
   * @param depth (optional) bound on the recursion depth
   * @param local (optional) at the end staturate without generating new terms
   * @return applications of the axioms
   */
  def saturate(depth: Option[Int] = None, local: Boolean = true) = {
    val buffer = scala.collection.mutable.ListBuffer[Formula]()
    var d = depth
    var old = -1
    while (d.getOrElse(1) > 0 && buffer.size > old) {
      old = buffer.size
      Logger("GuidedGenerator", Debug, "saturate at depth = " + d)
      buffer ++= generateWithExistingGTS
      d = d.map(_ - 1)
    }
    if (local && d.getOrElse(1) == 0) {
      Logger("GuidedGenerator", Debug, "saturate before local: " + buffer.size + " new clauses")
      buffer ++= gen.locallySaturate
    }
    Logger("GuidedGenerator", Debug, "saturate generated " + buffer.size + " new clauses")
    buffer.result
  }

  override def clone = {
    val cc2 = cc.copy
    val ig = new GuidedGenerator(f, cc2)
    ig.gen = gen.clone(cc2)
    ig
  }
  
  def log(lvl: Level) {
    Logger("GuidedQuantifier", lvl,
        "Guided Generator:\n  " + 
        FormulaUtils.getConjuncts(f).mkString("\n  ") + "\n" +
        "leftOver:\n  " + leftOver.mkString("\n  ") + "\n" +
        "CC\n" + cc + "\n")
    gen.log(lvl)
  }

}

protected class GGen(val vs: Array[Variable],
                     val f: Formula,
                     val newTerms: Array[Seq[Formula]],
                     val done: Array[Set[Formula]]) extends Cloneable {

  override def toString = vs.mkString("GGen( ",", "," → " + f)

  override def hashCode = f.hashCode

  override def clone = new GGen(vs, f, newTerms, done.clone)

  def similar(tg: GGen) = {
    tg.vs.size == vs.size &&
    tg.f == f &&
    (0 until tg.vs.size).forall(i => tg.vs(i) == vs(i))
  }

  def isResult = vs.isEmpty
  def result = {
    assert(isResult)
    f
  }

  protected def newGen(idx: Int, term: Formula, cc: CC): GGen = {
    assert(!done(idx)(term), "already done: " + idx + " → " + term)
    done(idx) = done(idx) + term
    def newIdx(i: Int): Int = if (i < idx) i else i + 1
    val kept = Array.ofDim[Variable](vs.size - 1)
    val subs = FormulaUtils.replace(vs(idx), term, f)
    val newT = Array.ofDim[Seq[Formula]](vs.size - 1)
    val done2 = Array.ofDim[Set[Formula]](vs.size - 1)
    for (i <- 0 until kept.size) {
      kept(i) = vs(newIdx(i))
      newT(i) = newTerms(i).flatMap( f => {
        val f2 = FormulaUtils.replace(vs(idx), term, f)
        if (f2.freeVariables.exists( kept contains _ )) Some(f2) else None
      })
      done2(i) = done(newIdx(i))
    }
    GGen(kept, subs, newT, done2, cc)
  }

  def apply(term: Formula, cc: CC): Iterable[GGen] = {
    var i = 0
    var res = List.empty[GGen]
    while(i < vs.size) {
      //check the type
      if (term.tpe == vs(i).tpe && !done(i)(term)) {
        //check the newly generated terms
        val nt = newTerms(i).map(FormulaUtils.replace(vs(i), term, _))
        def hasVarLeft(f: Formula) = {
          f.freeVariables.exists( v => vs.forall( v == _ ) )
        }
        if (nt.forall(hasVarLeft) ||
            nt.exists( t => !hasVarLeft(t) && cc.contains(t)))
        {
          res ::= newGen(i, term, cc)
        }
      }
      i += 1
    }
    res
  }
  
  /*
  def apply(v: Variable, term: Formula): GGen = {
    val i = vs.indexOf(v)
    newGen(i, term)
  }
  */

  def localMatches(cc: CC): Set[Map[Variable,Formula]] = {
    val toIdx = vs.zipWithIndex.toMap
    Matching.findLocalSubterms(cc, vs.toSet, f)
  }

}

protected object GGen {
    
  //assumes that we can reuse/modifiy the arrays
  def apply(vs: Array[Variable],
            f: Formula,
            newTerms: Array[Seq[Formula]],
            done: Array[Set[Formula]],
            cc: CC): GGen = {
    if (vs.isEmpty) {
      f match {
        case Exists(vse, fe) =>
          val bvse = vse.toSet
          val renaming = vse.map( v => {
            val defs = Gen.getDefinitions(v, fe)
            (v -> Gen.findCandidate(v, bvse - v, defs, cc, MMap[Binding,Variable]())) //TODO compDef
          }).toMap
          def map(f: Formula) = f match {
            case v @ Variable(_) => renaming.getOrElse(v, v)
            case f => f
          }
          FormulaUtils.map(map, fe) match {
            case ForAll(va, fa) => apply(va, fa, cc)
            case other => apply(vs, other, cc)
          }
        case other =>
          new GGen(vs, other, newTerms, done)
      }
    } else {
      Simplify.deBruijnIndexWithRenaming(ForAll(vs.toList, f)) match {
        case (ForAll(va, fa), renaming) =>
          import FormulaUtils._
          val revAlpha = renaming.map{ case (k,v) => (v->k) }
          val ava = va.toArray.sorted
          def oldIdx(i: Int): Int = {
            val oldV = revAlpha(ava(i))
            vs.indexOf(oldV)
          }
          val nt1 = newTerms.map( _.map( FormulaUtils.alpha(renaming, _) ) )
          val nt2 = Array.tabulate(ava.size)( i => nt1(oldIdx(i)) )
          val done2 = Array.tabulate(ava.size)( i => done(oldIdx(i)) )
          new GGen(ava, Simplify.simplify(fa), nt2, done2)
        case (other, _) =>
          apply(Array[Variable](), other, Array[Seq[Formula]](), Array[Set[Formula]](), cc)
      }
    }
  }
  
  def apply(vs: Iterable[Variable], f: Formula, cc: CC): GGen = {
    val avs = vs.toArray
    val tgs = Matching.termsGeneratedBy(vs.toSet, f)
    val nts = Array.tabulate(vs.size)( i => tgs.filter(_.freeVariables contains avs(i)).toSeq )
    val done = Array.fill(vs.size)(Set.empty[Formula])
    apply(avs, f, nts, done, cc)
  }

}
