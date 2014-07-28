package round.verification

import Utils._

import round._
import round.formula._

import round.utils.Logger
import round.utils.LogLevel._

import dzufferey.report._

class Verifier[IO](val alg: Algorithm[IO], dummyIO: IO) {

  //TODO make sure the specs are well-formed
  val spec = alg.spec 

  val process = alg.process(0, dummyIO)
  var procInitState: Formula = process.initState
  var procLocalVars: Set[Variable] = process.localVariables.toSet
  var procGhostVars: Set[Variable] = process.ghostVariables.toSet

  var roundsTR = process.rounds.map( r => (r.rawTR, r.auxSpec) )

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
    
    lst.add(itemForFormula("Initial state", procInitState))

    val rnds = new List("Rounds")
    for ( i <- process.rounds.indices ) {
      val lst = new List("Round " + i)
      lst.add(new PreformattedText("Send", process.rounds(i).sendStr))
      lst.add(new PreformattedText("Update", process.rounds(i).updtStr))
      val tr = process.rounds(i).rawTR
      val aux =  process.rounds(i).auxSpec
      val f = tr.makeFullTr(procLocalVars ++ procGhostVars, aux)
      val fs = FormulaUtils.getConjunts(f)
      lst.add(itemForFormula("Transition Relation", fs))
      //TR variables
      lst.add(new Text("Pre Variables", tr.old.mkString(", ")))
      lst.add(new Text("Local Variables", tr.local.mkString(", ")))
      lst.add(new Text("Post Variables", tr.primed.mkString(", ")))
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
    vcs.par.foreach(_.par.foreach(_.solve))
    //generate a report:
    
    val status = if (vcs.forall(_.exists(_.isValid))) " (success)" else " (failed)"
    val report = new Report("Verification of " + alg.getClass.toString + status)

    report.add(new PreformattedText("Code Before Processing", process.beforeProcessing))
    report.add(new PreformattedText("Code After Processing", process.afterProcessing))
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
