package round.verification

import Utils._

import round._
import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import dzufferey.report._

class Verifier[IO](val alg: Algorithm[IO], dummyIO: IO) {

  //TODO make sure the specs are well-formed
  val spec = alg.spec 

  val process = alg.process(new ProcessID(0), dummyIO)
  val procLocalVars: Set[Variable] = process.localVariables.toSet
  val procGhostVars: Set[Variable] = process.ghostVariables.toSet
  val procAllVars = procLocalVars ++ procGhostVars ++ process.globalVariables

  assert(procAllVars.forall(_.tpe != Wildcard))

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

  val additionalAxioms = alg.axiomList

  def checkProgress(
        descr: String,
        invariant1: Formula,
        invariant2: Formula,
        round: (RoundTransitionRelation,Map[String,AuxiliaryMethod])
      ): VC = {
    val withPost = round._1.makeFullTr(procLocalVars ++ procGhostVars, round._2)
    new VC("progress of " + descr, invariant1, withPost, round._1.primeFormula(invariant2))
  }

  def checkInductiveness(
        descr: String,
        invariant: Formula,
        round: (RoundTransitionRelation,Map[String,AuxiliaryMethod])
      ): VC = {
    val tr = round._1
    val withPost = tr.makeFullTr(procLocalVars ++ procGhostVars, round._2)
    new VC("inductiveness of " + descr, invariant, withPost, tr.primeFormula(invariant))
  }

  /* for each sublist, at least one VC has to hold. */
  def generateVCs: Seq[Seq[VC]] = {

    //1st invariant is implied by initial state
    val initVC = new VC("initial state implies invariant 0",
                        ForAll(List(procI), localize(procLocalVars ++ procGhostVars, procI, procInitState)),
                        True(),
                        removeInitPrefix(removeOldPrefix(spec.invariants(0))))

    //invariants are inductive
    val inductVCs =
      for ( (inv, idx) <- spec.invariants.zipWithIndex;
            r <- roundsTR.indices)
       yield checkInductiveness("invariant " + idx + " at round " + r, inv, roundsTR(r))
    //-magic round => from one invariant to the next one
    val pairedInvs = spec.invariants.sliding(2).filter(_.length >= 2).toList
    val progressVCs =
      for ( (invs, idx) <- pairedInvs.zipWithIndex ) yield {
        for (r <- roundsTR.indices) yield checkProgress("progress from " + idx + " to " + (idx+1) + " at round " + r,
                                                        invs(0),
                                                        invs(1),
                                                        roundsTR(r))
      }
    //TODO increment of r

    //invariants => properties
    val propertiesVCs =
      for ( (name, formula) <- spec.properties ) yield {
        for ( (inv, idx) <- spec.invariants.zipWithIndex) yield {
          new VC("invariant " + idx + " implies " + name, inv, True(), formula)
        }
      }

    //TODO auxiliaryFunction preconditions
    Logger("Verifier", Warning, "TODO: preconditions of auxiliary methods")

    //pack everything
    List(initVC) :: inductVCs.map(List(_)) ::: progressVCs ::: propertiesVCs
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
    for( a <- additionalAxioms )
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
    vcs.foreach(_.foreach(_.solve))
    //generate a report:
    
    val status = if (vcs.forall(_.exists(_.isValid))) " (success)" else " (failed)"
    val report = new Report("Verification of " + alg.getClass.toString + status)

    //report.add(new Code("Code Before Processing", process.beforeProcessing))
    report.add(new Code("Code After Processing", process.afterProcessing))
    report.add(reportSpec)
    report.add(reportProcess)

    val rVcs = new Sequence("Verification Conditions")
    for ( (vs, idx) <- vcs.zipWithIndex) {
      val status = if (vs.exists(_.isValid)) " (success)" else " (failed)"
      val lst = new List("VCs group " + idx + status)
      for (v <- vs) lst.add(v.report)
      rVcs.add(lst)
    }
    report.add(rVcs)

    report
  }

}
