package round.verification

import Utils._

import round._
import round.formula._

import round.utils.Logger
import round.utils.LogLevel._

import round.utils.report._

class Verifier[IO](val alg: Algorithm[IO], dummyIO: IO) {

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
    val withPost = round._1.makeFullTr(procLocalVars ++ procGhostVars, round._2)
    new VC("inductiveness of " + descr, invariant, withPost, round._1.primeFormula(invariant))
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
    val pairedInvs = spec.invariants.sliding(2).filter(_.length < 2).toList
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
    List(List(initVC), inductVCs) ::: progressVCs ::: propertiesVCs
  }

  def check: Report = {
    val vcs = generateVCs
    //solve the queries
    vcs.par.foreach(_.par.foreach(_.solve))
    //TODO generate a report:
    val report = new Report("Verification of " + alg.getClass.toString)
    //-list of the specs, state, ...
    //-the VCs: if sat then try to give a model
    sys.error("TODO")
  }

}
