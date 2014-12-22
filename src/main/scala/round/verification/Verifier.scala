package round.verification

import Utils._

import round._
import round.formula._

import dzufferey.utils.Namer
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import dzufferey.report._

class Verifier[IO](val alg: Algorithm[IO]) {

  val spec = alg.spec 

  val process = alg.process
  val procLocalVars: Set[Variable] = process.localVariables.toSet
  val procGhostVars: Set[Variable] = process.ghostVariables.toSet
  val procAllVars = procLocalVars ++ procGhostVars ++ process.globalVariables

  assert(procAllVars.forall(_.tpe != Wildcard))

  //to avoid capture during later renaming
  def warmup {
    Logger("Verifier", Debug, "Warming up the Namer")
    def w(f: Formula) {
      FormulaUtils.collectVariables(f).foreach(v => Namer.warmup(v.name))
    }
    def wv(v: Variable) { Namer.warmup(v.name) }

    for( v <- procAllVars) wv(v)
    w(spec.safetyPredicate)
    spec.livnessPredicate.foreach(w)
    spec.invariants.foreach(w)
    spec.properties.map(_._2).foreach(w)
    w(process.initState)
    for(r <- process.rounds) {
      val t = r.rawTR
      w(t.send)
      w(t.update)
      wv(t.mailboxSend)
      wv(t.mailboxUpdt)
      t.old.foreach(wv)
      t.local.foreach(wv)
      t.primed.foreach(wv)
      for( a <- r.auxSpec.values ) {
        a.params.foreach(wv)
        w(a.pre)
        a.body.foreach( tr => {
          (tr.old ++ tr.local ++ tr.primed).foreach(wv)
          w(tr.tr)
        })
        a.post.map( p => {
          wv(p._1)
          w(p._2)
        })
      }
    }
  }
  warmup

  val procInitState: Formula = {
    //fix the types!
    def fillType(f: Formula) = f match {
      case v: Variable if procAllVars contains v =>
        val v2 = procAllVars.find(_ == v).get
        assert(Typer.unify(v.tpe, v2.tpe).isDefined, "v.tpe = " + v.tpe + ", v2.tpe = " + v2.tpe)
        v.setType(v2.tpe)
      case _ => ()
    }
    FormulaUtils.traverse( fillType, process.initState)
    val f2 = Typer(process.initState).get
    def guessType1(f: Formula) {
      f.tpe match {
        case TypeVariable(v) =>
          Logger("Verifier", Warning, "guessing type for " + f + ": " + f.tpe)
          f.setType(UnInterpreted("u"+v))
        case _ => ()
      }
    }
    def guessType2(f: Formula) = f match {
      case Application(UnInterpretedFct(fct, None, Nil), args) =>
        val concreteType = Function(args.map(_.tpe), f.tpe)
        Logger("Verifier", Warning, "guessing type for " + fct + ": " + concreteType)
        Application(UnInterpretedFct(fct, Some(concreteType), Nil), args)
      case other => other
    }
    FormulaUtils.traverse( guessType1, f2)
    FormulaUtils.map( guessType2, f2)
  }

  var roundsTR = process.rounds.map( r => (r.rawTR.retype(procAllVars), r.auxSpec) )

  val additionalAxioms = alg.axiomList.map(_.formula)

  def roundInvariant(invIdx: Int, roundIdx: Int) = {
    val idx2 = roundIdx - 1
    if (spec.roundInvariants.length > idx2 &&
        spec.roundInvariants(idx2).length > invIdx) {
      spec.roundInvariants(idx2)(invIdx)
    } else {
      True()
    }
  }

  def getInvariantforProgress(invIdx: Int, roundIdx: Int) = {
    val phaseInv = spec.invariants(invIdx)
    if (roundIdx == 0) {
      phaseInv //start of phase: normal invariant
    } else if (roundIdx == roundsTR.length) {
      spec.invariants(invIdx + 1) //end of phase: new invariant
    } else {
      assert(roundIdx > 0 && roundIdx < roundsTR.length)
      //progress: grab the roundInvariant of invIdx + 1
      And(phaseInv, roundInvariant(invIdx + 1, roundIdx))
    }
  }

  def getInvariant(invIdx: Int, roundIdx: Int) = {
    val phaseInv = spec.invariants(invIdx)
    if (roundIdx == 0 || roundIdx == roundsTR.length) {
      phaseInv
    } else {
      assert(roundIdx > 0 && roundIdx < roundsTR.length)
      And(phaseInv, roundInvariant(0, roundIdx))
    }
  }

  def checkProgress(
        invIdx: Int, //from invIdx to invIdx+1
        roundIdx: Int
      ): VC = {
    val env = spec.livnessPredicate(invIdx)
    val round = roundsTR(roundIdx)
    val invariant1 = getInvariantforProgress(invIdx, roundIdx)
    val invariant2 = getInvariantforProgress(invIdx, roundIdx + 1)
    val preInv = FormulaUtils.alpha(Map(r -> rp), invariant1)
    val withPost = mkTR(round._1, round._2, Some(env))
    val postInv = round._1.primeFormula(invariant2)
    val descr = "progress from " + invIdx + " to " + (invIdx+1) + " at round " + roundIdx
    new SingleVC(descr, preInv, withPost, postInv, additionalAxioms)
  }

  def mkTR(tr: RoundTransitionRelation, aux: Map[String, AuxiliaryMethod], env: Option[Formula]) = {
    val normal =
      And( And( Eq(r, Plus(Literal(1),rp)),
                spec.safetyPredicate ),
           tr.makeFullTr(procLocalVars ++ procGhostVars, aux) )
    env match {
      case Some(f) => And(f, normal)
      case None => normal
    }
  }

  def checkInductiveness(
        invIdx: Int,
        roundIdx: Int
      ): VC = {
    val round = roundsTR(roundIdx)
    val tr = round._1
    val preInv = FormulaUtils.alpha(Map(r -> rp), getInvariant(invIdx, roundIdx))
    val postInv = tr.primeFormula(getInvariant(invIdx, roundIdx+1))
    val withPost = mkTR(tr, round._2, None)
    val descr = "inductiveness of invariant " + invIdx + " at round " + roundIdx
    new SingleVC(descr, preInv, withPost, postInv, additionalAxioms)
  }

  def checkProperty(
        descr: String,
        invariant: Formula,
        property: Formula
      ): VC = {
    //check whether we have a state or relational property
    if (isStateProperty(property)) {
      new SingleVC(descr, invariant, True(), property, additionalAxioms)
    } else if (isRelationalProperty(property)) {
      new CompositeVC(descr, true,
        for (r <- roundsTR.indices) yield {
          val (tr, aux) = roundsTR(r)
          val f = And(invariant, property)
          new SingleVC(
            "relational property preserved at round " + r,
            f,//TODO this might still contains old terms, they are harmless
            tr.makeFullTr(procLocalVars ++ procGhostVars, aux),
            tr.primeFormula(f),
            additionalAxioms
          )
        }
      )
    } else if (isGlobalProperty(property)) {
      new CompositeVC(descr, true,
        new SingleVC(
          "global property hold initially",
          ForAll(List(procI), localize(procLocalVars ++ procGhostVars, procI, procInitState)),
          Eq(r, Literal(0)),
          removeInitPrefix(property),
          additionalAxioms
        ) +:
        roundsTR.indices.map( r => {
          val (tr, aux) = roundsTR(r)
          val f = And(invariant, property)
          new SingleVC(
            "global property preserved at round " + r,
            f,
            tr.makeFullTr(procLocalVars ++ procGhostVars, aux),
            tr.primeFormula(f),
            additionalAxioms
          )
        })
      )
    } else {
      sys.error("unknown type of property: " + property)
    }
  }

  /* for each sublist, at least one VC has to hold.
   * TODO better structure
   */
  def generateVCs: Seq[VC] = {

    //1st invariant is implied by initial state
    val initVC = new SingleVC(
      "Initial state implies invariant 0",
      ForAll(List(procI), localize(procLocalVars ++ procGhostVars, procI, procInitState)),
      Eq(r, Literal(0)),
      removeInitPrefix(removeOldPrefix(spec.invariants(0))),
      additionalAxioms
    )

    //invariants are inductive
    val inductVCs: scala.List[VC] =
      for ( idx <- (0 until spec.invariants.length).toList;
            r <- roundsTR.indices)
       yield checkInductiveness(idx, r)

    //-magic round => from one invariant to the next one
    val progressVCs: scala.List[VC] =
      for ( idx <- (0 until spec.invariants.length - 1).toList) yield {
        new CompositeVC("progress from " + idx + " to " + (idx+1), true,
          for (r <- roundsTR.indices)
            yield checkProgress(idx, r)
        )
      }


    //invariants => properties
    val propertiesVCs: scala.List[VC] =
      for ( (name, formula) <- spec.properties ) yield {
        new CompositeVC("property: " + name, false,
          for ( (inv, idx) <- spec.invariants.zipWithIndex) yield {
            checkProperty("invariant " + idx + " implies " + name, inv, formula)
          }
        )
      }

    //TODO auxiliaryFunction preconditions
    Logger("Verifier", Warning, "TODO: preconditions of auxiliary methods")

    //pack everything
    initVC :: inductVCs ::: progressVCs ::: propertiesVCs
  }


  def reportSpec: Item = {
    val lst = new Sequence("Specification")

    lst.add(itemForFormula("Safety Predicate", spec.safetyPredicate))

    val liveness = new List("Liveness Predicates")
    for( (f,i) <- spec.livnessPredicate.zipWithIndex )
      liveness.add(itemForFormula(i.toString, f))
    lst.add(liveness)

    val invs = new List("Invariants")
    for( (f,i) <- spec.invariants.zipWithIndex )
      invs.add(itemForFormula(i.toString, f))
    lst.add(invs)

    val props = new List("Properties")
    for( (str, f) <- spec.properties )
      props.add(itemForFormula(str, f))
    lst.add(props)

    val axioms = new List("Additional Axioms")
    for( a <-  alg.axiomList)
      axioms.add(itemForFormula(a.name, a.formula))
    lst.add(axioms)

    lst
  }


  def reportProcess: Item = {
    val lst = new Sequence("Process")

    val vars = new List("Variables")
    vars.add(new Text("Global", process.globalVariables.map(v => v.name+": " +v.tpe).mkString(", ")))
    vars.add(new Text("Local", process.localVariables.map(v => v.name+": " +v.tpe).mkString(", ")))
    vars.add(new Text("Ghost", process.ghostVariables.map(v => v.name+": " +v.tpe).mkString(", ")))
    lst.add(vars)
    
    lst.add(itemForFormula("Initial state", procInitState))

    val rnds = new List("Rounds")
    for ( i <- process.rounds.indices ) {
      val lst = new List("Round " + i)
      lst.add(new Code("Send", process.rounds(i).sendStr))
      lst.add(new Code("Update", process.rounds(i).updtStr))
      val tr = roundsTR(i)._1
      val aux = roundsTR(i)._2
      val f = tr.makeFullTr(procLocalVars ++ procGhostVars, aux)
      val fs = FormulaUtils.getConjuncts(f)
      lst.add(itemForFormula("Transition Relation", fs))
      //TR variables
      lst.add(new Text("Pre Variables", tr.old.map(v => v.name+": " +v.tpe).mkString(", ")))
      lst.add(new Text("Local Variables", tr.local.map(v => v.name+": " +v.tpe).mkString(", ")))
      lst.add(new Text("Post Variables", tr.primed.map(v => v.name+": " +v.tpe).mkString(", ")))
      //auxiliary methods
      for ( a <- aux.values ) lst.add(a.report)
      rnds.add(lst)
    }
    lst.add(rnds)

    lst
  }

  def check: Report = {
    val vcs = generateVCs
    //solve the queries
    //vcs.par.foreach(_.par.foreach(_.solve))
    vcs.foreach(_.solve)

    //generate a report:
    val status = if (vcs.forall(_.isValid)) " (success)" else " (failed)"
    val report = new Report("Verification of " + alg.getClass.toString + status)

    //report.add(new Code("Code Before Processing", process.beforeProcessing))
    report.add(new Code("Code After Processing", process.afterProcessing))
    report.add(reportSpec)
    report.add(reportProcess)

    val rVcs = new Sequence("Verification Conditions")
    for ( vc <- vcs) {
      rVcs.add(vc.report)
    }
    report.add(rVcs)

    //TODO better way to do that
    round.utils.smtlib.Solver.executor.shutdown

    report
  }

}

object Verifier {

  def apply(className: String): Verifier[_] = {
    val str = "new round.verification.Verifier(new " + className + ")"
    import scala.reflect.runtime.universe._
    import scala.tools.reflect.ToolBox
    val tb = runtimeMirror(scala.reflect.runtime.universe.getClass.getClassLoader).mkToolBox()
    val tree = tb.parse(str)
    Logger("Verifier", Info, "executing: " + tree)
    tb.compile(tree)().asInstanceOf[Verifier[_]]
  }

}
