package psync.logic.quantifiers

import psync.formula._
import psync.logic._
import psync.utils.Options

import dzufferey.utils.{Misc, Namer}
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.ArrayBuffer

class IncrementalGenerator( axioms: Iterable[Formula],
                            tactic: Tactic,
                            _cc: CongruenceClosure
                          ) extends Generator {

  def cc = _cc
  
  val _logger = {
    if (Options.logQI) new BasicQILogger
    else new EmptyQILogger
  }
  def logger = _logger

  def saturate(depth: Option[Int], local: Boolean): List[Formula] = {
    val buffer = scala.collection.mutable.Set[Formula]()

    tactic.init(depth.getOrElse(1000), cc)
    
    while (tactic.hasNext) {
      val t = tactic.next
      val result = generate(t)
      tactic.generatorResult(result)
    }

    buffer ++= tactic.result
    tactic.clear

    if (local) buffer ++= locallySaturate

    if (Options.logQI) {
      val fname = Namer("qi") + ".html"
      logger.storeVisJS(fname)
    }

    buffer.toList
  }

  //the current generators
  protected val idx  = MMap[Type,ArrayBuffer[Int]]()
  protected val gens = ArrayBuffer[Gen]()
  
  //speed-up the findSimilar test by keeping the hashes of existing generators
  protected val hashFilter = MMap[Int,ArrayBuffer[Int]]()
  
  //for limiting the number of ∃ var generated for comprehensions
  protected val compDef = MMap[Binding,Variable]()

  //for leave nodes in QILogger
  protected var leafCounter = 0
  protected def newLeadIdx = {
    leafCounter -= 1
    leafCounter
  }

  /* returns an existing similar Gen if there already is one */
  protected def findSimilar(g: Gen): Option[Int] = {
    val potentialConflict = hashFilter.getOrElseUpdate(g.hashCode, ArrayBuffer[Int]())
    potentialConflict.find( i => gens(i).similar(g))
  }
  
  /* adds a new generator to the list of generator */
  protected def addGen(g: Gen) = {
    assert(!g.isResult)
    findSimilar(g) match {
      case None =>
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
        logger.addNode(index, ForAll(g.vs.toList, g.f),
                       FormulaUtils.collectGroundTerms(ForAll(g.vs.toList, g.f)) -- cc.groundTerms)
        index
      case Some(index) =>
        index
    }
  }

  //extract the first Gen from the axioms
  axioms.foreach{
    case fa @ ForAll(vs, f) => addGen( makeGen(vs, f) )
    case other => Logger("IncrementalGenerator", Warning, "(2) expect ∀, found: " + other)
  }

  def generate(term: Formula): List[Formula] = {
    val candidate = idx.getOrElseUpdate(term.tpe, ArrayBuffer[Int]())
    var i = 0
    val res = MSet[Formula]()
    val newGens = scala.collection.mutable.Stack[Gen]()
    while(i < candidate.size) {
      val c = candidate(i)
      val s = gens(c).vs.size
      newGens.pushAll( gens(c)(term) ) //this could be cut short by directly giving a ref to the stack
      while(!newGens.isEmpty) {
        val g = newGens.pop
        if (g.isResult) {
          if (g.result != True()) {
            if (!res(g.result)) {
              val l = newLeadIdx
              logger.addNode(l, g.result,
                             FormulaUtils.collectGroundTerms(g.result) -- cc.groundTerms)
              logger.addEdge(c, l, term)
            }
            res += g.result
          }
        } else {
          val d = addGen(g)
          logger.addEdge(c, d, term)
        }
      }
      i += 1
    }
    res.toList
  }

  def locallySaturate: List[Formula] = {
    var i = 0
    val lDone = scala.collection.mutable.BitSet()
    val res = MSet[Formula]()
    def checkDone(g: Gen, remaining: Iterable[Map[Variable,Formula]]) = {
      if (g.isResult) {
        true
      } else {
        false
      }
    }
    def instVar(idx: Int, g: Gen, matches: Iterable[Map[Variable,Formula]]) {
      assert(!g.isResult)
      val v = g.vs.last
      //println(matches.mkString(g + "\n  ", "\n  ", ""))
      val byV = matches.filter(_ contains v).groupBy(_(v)) //matches without v are term generating due to existential quantifiers
      for ( (candidate, maps) <- byV ) {
        val g2 = g(v, candidate)
        if (g2.isResult) {
          if (g2.result != True()) {
            if (!res(g2.result)) {
              val l = newLeadIdx
              logger.addNode(l, g2.result,
                             FormulaUtils.collectGroundTerms(g2.result) -- cc.groundTerms)
              logger.addEdge(idx, l, candidate)
            }
            res += g2.result
          }
        } else {
          val renaming = g.vs.zip(g2.vs).toMap //g2 has renamed arguments ...
          val remaining = maps.map( m => {
            val m1 = m - v 
            m1.map{ case (a,b) => renaming(a) -> b }
          })
          val newIdx = addGen(g2)
          logger.addEdge(idx, newIdx, candidate)
          val g3 = gens(newIdx) //in case we add state to the gen
          lDone += newIdx
          instVar(newIdx, g3, remaining)
        }
      }
    }
    while(i < gens.size && !lDone(i)) {
      val g = gens(i)
      val matches = g.localMatches
      instVar(i, g, matches)
      i += 1
    }
    res.toList
  }

  def clone(cc2: CongruenceClosure) = {
    val g = new IncrementalGenerator(axioms, tactic, cc2)
    g.idx.clear
    idx.foreach{ case (k,v) => g.idx += (k -> v.clone) }
    g.gens.clear
    gens.foreach( gen => g.gens.append(g.copyGen(gen.vs, gen.f)))
    g.hashFilter.clear
    hashFilter.foreach{ case (k,v) =>
      g.hashFilter += (k -> v.clone)
    }
    g.compDef.clear
    compDef.foreach( g.compDef += _ )
    g
  }

  def log(lvl: Level) {
    Logger("IncrementalGenerator", lvl, {
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

  protected def mkGenVar(v: Variable) = {
    val suffix = psync.utils.smtlib.Names.tpe(v.tpe)
    Variable(Namer("_genExt_"+suffix)).setType(v.tpe)
  }
  
  protected def getDefinitions(v: Variable, f: Formula): Seq[Formula] = f match {
    case ForAll(fa, f2) =>
      val lst = getDefinitions(v, f2)
      val fas = fa.toSet
      lst.filter( l => fas.intersect(l.freeVariables).isEmpty )
    case And(lst @ _*) => lst.flatMap(getDefinitions(v, _))
    case Eq(`v`, e) => Seq(e)
    case Eq(e, `v`) => Seq(e)
    case _ => Seq()
  }
  
  protected def findCandidate(v: Variable, bvs: Set[Variable], defs: Seq[Formula]): Formula = {
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
                val v2 = mkGenVar(v)
                compDef += (c -> v2)
                Some(v2)
              }
            case _ => None
          }
          if (candidates.isEmpty) mkGenVar(v)
          else candidates.head
        case _ => mkGenVar(v)
      }
    } else {
      cc.repr(defs2.head)
    }
  }
  
  protected def copyGen(vs: Array[Variable], f: Formula): Gen = {
    new Gen(vs, f)
  }
  
  //check ∃∀: if the top level is ∃, then give it a fresh name and continue
  protected def makeGen(vs0: List[Variable], f0: Formula): Gen = {
    val f = Simplify.simplify(f0)
    val vs = vs0.filter(f.freeVariables)
    if (vs.isEmpty) {
      f match {
        case Exists(vse, fe) =>
          val bvse = vse.toSet
          val renaming = vse.map( v => {
            val defs = getDefinitions(v, fe)
            (v -> findCandidate(v, bvse - v, defs)) 
          }).toMap
          def map(f: Formula) = f match {
            case v @ Variable(_) => renaming.getOrElse(v, v)
            case f => f
          }
          FormulaUtils.map(map, fe) match {
            case ForAll(va, fa) => makeGen(va, fa)
            case other => makeGen(Nil, other)
          }
        case other =>
          new Gen(Array.empty[Variable], other)
      }
    } else {
      Simplify.deBruijnIndex(ForAll(vs, f)) match {
        case ForAll(va, fa) =>
          import FormulaUtils._
          new Gen(va.toArray.sorted, Simplify.simplify(fa))
        case other => makeGen(Nil, other)
      }
    }
  }


  protected class Gen(val vs: Array[Variable], val f: Formula) {

    override def toString = vs.mkString("Gen( ",", "," → " + f)

    override def hashCode = f.hashCode

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
      makeGen(kept, subs)
    }

    def apply(term: Formula): Iterable[Gen] = {
      var i = 0
      var res = List.empty[Gen]
      while(i < vs.size) {
        if (term.tpe == vs(i).tpe) {
          res ::= newGen(i, term)
        }
        i += 1
      }
      res
    }
    
    def apply(v: Variable, term: Formula): Gen = {
      val i = vs.indexOf(v)
      newGen(i, term)
    }

    def localMatches: Set[Map[Variable,Formula]] = {
      val toIdx = vs.zipWithIndex.toMap
      Matching.findLocalSubterms(cc, vs.toSet, f)
    }

  }

}

