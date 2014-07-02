package round.verification

import round._
import round.formula._

class Verifier[IO](val alg: Algorithm[IO], dummyIO: IO) {

  val spec = alg.spec 

  var procInitState: Formula = True()

  var roundsTR: Array[(TransitionRelation,Map[String,AuxiliaryMethod])] = Array()

  val additionalAxioms = alg.axiomList

  def gatherSpecs = {
    val process = alg.process(0, dummyIO)
    procInitState = process.initState
    roundsTR = process.rounds.map( r => (r.rawTR, r.auxSpec) )
  }

  def generateVC = {
    //-1st invariant is implied by initial state
    //-invariants are inductive
    //-magic round => from one invariant to the next one
    //-invariants => properties
    sys.error("TODO")
  }

  //TODO generate a report:
  //-list of the specs, state, ...
  //-the VCs: if sat then try to give a model

}
