package psync.logic

import psync.formula._

import dzufferey.utils.{Misc, Namer}
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.ArrayBuffer


class IncrementalFormulaGenerator(axioms: Iterable[Formula], cc: CongruenceClosure) {

  //the current generators
  protected val idx  = scala.collection.mutable.Map[Type,ArrayBuffer[Int]]()
  protected val gens = ArrayBuffer[Gen]()
  protected val done = scala.collection.mutable.Map[Type,MSet[Formula]]()
  
  //speed-up the findSimilar test by keeping the hashes of existing generators
  protected val hashFilter = scala.collection.mutable.Map[Int,ArrayBuffer[Int]]()
  
  //for limiting the number of ∃ var generated for comprehensions
  protected val compDef = scala.collection.mutable.Map[Binding,Variable]()
  

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
    case fa @ ForAll(vs, f) => addGen( Gen(vs, f, cc, compDef) )
    case other => Logger("IncrementalGenerator", Warning, "(2) expect ∀, found: " + other)
  }
  

  def generate(term: Formula): List[Formula] = {
    val did = done.getOrElseUpdate(term.tpe, MSet[Formula]())
    if (!did(term)) {
      val candidate = idx.getOrElseUpdate(term.tpe, ArrayBuffer[Int]())
      var i = 0
      val res = MSet[Formula]()//List.empty[Formula]
      val newGens = scala.collection.mutable.Stack[Gen]()
      while(i < candidate.size) {
        val c = candidate(i)
        val s = gens(c).vs.size
        newGens.pushAll( gens(c)(term, cc, compDef) ) //this could be cut short by directly giving a ref to the stack
        while(!newGens.isEmpty) {
          val g = newGens.pop
          if (g.isResult) {
            if (g.result != True()) res += g.result
          } else if (g.vs.size >= s) {
            //for generator that just got an ∃ instantiated, we need to look for terms which have already been processed
            val j = 0
            val tpeDone = MSet[Type]()
            while (j < g.vs.size && !tpeDone(g.vs(j).tpe)) {
              val catchUP = done.getOrElseUpdate(g.vs(j).tpe, MSet[Formula]())
              tpeDone += g.vs(j).tpe
              for ( c <- catchUP ) newGens.pushAll( g(c, cc, compDef) ) //this could be cut short by directly giving a ref to the stack
            }
            addGen(g)
          } else {
            assert(g.vs.size + 1 == s, "old: " + gens(c) + ", new: " + g)
            addGen(g)
          }
        }
        i += 1
      }
      did += term
      res.toList
    } else {
      Nil
    }
  }
  
  def generate(groundTerms: Iterable[Formula]): List[Formula] = {
    groundTerms.toList.flatMap(generate)
  }

  //TODO that part should not change the internal state of the generator, nor add any new term in the CC (be careful about ∃)
  def locallySaturate: List[Formula] = {
    var i = 0
    val lDone = scala.collection.mutable.BitSet()
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
        for ( (candidate, maps) <- byV ) {
          val g2 = g(v, candidate, cc, compDef)
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
          lDone += i
          instVar(g2, remaining)
        case None =>
          lDone += gens.size
          addGen(g)
          instVar(g, remaining)
      }
    }
    def notDone(m: Map[Variable,Formula]) = {
      m.forall{ case (v, f) =>
        if (done contains v.tpe) {
          val dt = done(v.tpe)
          !dt(f) && !dt(cc.repr(f))
        } else {
          true
        }
      }
    }
    while(i < gens.size && !lDone(i)) {
      val g = gens(i)
      val matches = g.localMatches(cc).filter(notDone)
      instVar(g, matches)
      i += 1
    }
    res.result
  }

  def clone(cc2: CongruenceClosure) = {
    val g = new IncrementalFormulaGenerator(axioms, cc2)
    g.idx.clear
    idx.foreach{ case (k,v) => g.idx += (k -> v.clone) }
    g.gens.clear
    gens.foreach( gen => g.gens.append(gen.clone) )
    g.hashFilter.clear
    hashFilter.foreach{ case (k,v) =>
      g.hashFilter += (k -> v.clone)
    }
    g.compDef.clear
    compDef.foreach( g.compDef += _ )
    g
  }

  def log(lvl: Level) {
    Logger("IncrementalFormulaGenerator", lvl, {
      val buffer = new scala.collection.mutable.StringBuilder(1024 * 1024)
      buffer ++= "idx:\n"
      for ( (t, is) <- idx) {
        buffer ++= "  "
        buffer ++= t.toString
        buffer ++= " → "
        buffer ++= is.mkString(", ")
        buffer ++= "\n"
      }
      buffer ++= "gens:\n"
      for (i <- 0 until gens.size) {
        buffer ++= "  "
        buffer ++= i.toString
        buffer ++= ":  "
        buffer ++= gens(i).toString
        buffer ++= "\n"
      }
      buffer ++= "done:\n"
      for ( (t, fs) <- done) {
        buffer ++= "  "
        buffer ++= t.toString
        buffer ++= " → "
        buffer ++= fs.mkString(", ")
        buffer ++= "\n"
      }
      buffer ++= "hashFilter:\n"
      for ( (i, is) <- hashFilter) {
        buffer ++= "  "
        buffer ++= i.toString
        buffer ++= " → "
        buffer ++= is.mkString(", ")
        buffer ++= "\n"
      }
      buffer.toString
    })
  }

}


class IncrementalGenerator(f: Formula, val cc: CongruenceClosure = new CongruenceClosure) extends Cloneable {

  //make sure the current equalities are in the cc
  cc.addConstraints(f)

  var leftOver: List[Formula] = Nil
  protected var gen = {
    val (axioms, other) = FormulaUtils.getConjuncts(f).partition(Quantifiers.hasFAnotInComp)
    leftOver = other
    new IncrementalFormulaGenerator(axioms.map(Simplify.pnf), cc)
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
   * @param local (optional) at the end staturate without generating new terms
   * @return applications of the axioms
   */
  def saturate(depth: Option[Int] = None, local: Boolean = true) = {
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
    if (local && d.getOrElse(1) == 0) {
      Logger("IncrementalGenerator", Debug, "saturate before local: " + buffer.size + " new clauses")
      buffer ++= gen.locallySaturate
    }
    Logger("IncrementalGenerator", Debug, "saturate generated " + buffer.size + " new clauses")
    buffer.result
  }

  override def clone = {
    val cc2 = cc.copy
    val ig = new IncrementalGenerator(f, cc2)
    ig.gen = gen.clone(cc2)
    ig
  }
  
  def log(lvl: Level) {
    Logger("IncrementalGenerator", lvl,
        "Incremental Generator:\n  " + 
        FormulaUtils.getConjuncts(f).mkString("\n  ") + "\n" +
        "leftOver:\n  " + leftOver.mkString("\n  ") + "\n" +
        "CC\n" + cc + "\n")
    gen.log(lvl)
  }

}


protected class Gen(val vs: Array[Variable], val f: Formula) extends Cloneable {

  override def toString = vs.mkString("Gen( ",", "," → " + f)

  override def hashCode = f.hashCode

  override def clone = this

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

  def newGen(idx: Int, term: Formula, cc: CC, compMap: MMap[Binding,Variable]): Gen = {
    val kept = List.tabulate(vs.size -1)( i => if (i < idx) vs(i) else vs(i+1) )
    val subs = FormulaUtils.replace(vs(idx), term, f)
    Gen(kept, subs, cc, compMap)
  }

  def apply(term: Formula, cc: CC, compMap: MMap[Binding,Variable]): Iterable[Gen] = {
    var i = 0
    var res = List.empty[Gen]
    while(i < vs.size) {
      if (term.tpe == vs(i).tpe) {
        res ::= newGen(i, term, cc, compMap)
      }
      i += 1
    }
    res
  }
  
  def apply(v: Variable, term: Formula, cc: CC, compMap: MMap[Binding,Variable]): Gen = {
    val i = vs.indexOf(v)
    newGen(i, term, cc, compMap)
  }

  def localMatches(cc: CC): Set[Map[Variable,Formula]] = {
    val toIdx = vs.zipWithIndex.toMap
    Matching.findLocalSubterms(cc, vs.toSet, f)
  }

}

protected object Gen {

  def mkVar(v: Variable) = {
    val suffix = psync.utils.smtlib.Names.tpe(v.tpe)
    Variable(Namer("_genExt_"+suffix)).setType(v.tpe)
  }

  def getDefinitions(v: Variable, f: Formula): Seq[Formula] = f match {
    case ForAll(fa, f2) =>
      val lst = getDefinitions(v, f2)
      val fas = fa.toSet
      lst.filter( l => fas.intersect(l.freeVariables).isEmpty )
    case And(lst @ _*) => lst.flatMap(getDefinitions(v, _))
    case Eq(`v`, e) => Seq(e)
    case Eq(e, `v`) => Seq(e)
    case _ => Seq()
  }

  def findCandidate(v: Variable, bvs: Set[Variable], defs: Seq[Formula], cc: CC, compDef: MMap[Binding, Variable]): Formula = {
    //check if there is an existing ground term
    val defs1 = defs.filter( d => d.freeVariables.forall( fv => !bvs(fv)))
    val defs2 = defs1.filter( d => cc.contains(d) )
    if (defs2.isEmpty) {
      v.tpe match {
        case FSet(_) =>
          //the comprehensions are not kept in the cc
          val candidates = defs1.flatMap{
            case c @ Binding(Comprehension, _, _) =>
              if (compDef contains c) {
                Some(compDef(c))
              } else {
                val v2 = mkVar(v)
                compDef += (c -> v2)
                Some(v2)
              }
            case _ => None
          }
          if (candidates.isEmpty) mkVar(v)
          else candidates.head
        case _ => mkVar(v)
      }
    } else {
      cc.repr(defs2.head)
    }
  }

  //check ∃∀: if the top level is ∃, then give it a fresh name and continue
  def apply(vs: List[Variable], f: Formula, cc: CC, compDef: MMap[Binding, Variable]): Gen = {
    if (vs.isEmpty) {
      f match {
        case Exists(vse, fe) =>
          val bvse = vse.toSet
          val renaming = vse.map( v => {
            val defs = getDefinitions(v, fe)
            (v -> findCandidate(v, bvse - v, defs, cc, compDef)) 
          }).toMap
          def map(f: Formula) = f match {
            case v @ Variable(_) => renaming.getOrElse(v, v)
            case f => f
          }
          FormulaUtils.map(map, fe) match {
            case ForAll(va, fa) => apply(va, fa, cc, compDef)
            case other => apply(Nil, other, cc, compDef)
          }
        case other =>
          new Gen(Array.empty[Variable], other)
      }
    } else {
      Simplify.deBruijnIndex(ForAll(vs, f)) match {
        case ForAll(va, fa) =>
          import FormulaUtils._
          new Gen(va.toArray.sorted, Simplify.simplify(fa))
        case other => apply(Nil, other, cc, compDef)
      }
    }
  }

}
