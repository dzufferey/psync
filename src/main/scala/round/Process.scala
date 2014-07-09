package round

import Algorithm._

abstract class Process(val id: ProcessID) {

  val rounds: Array[Round]



  //////////////////
  // util methods //
  //////////////////

  def setGroup(g: round.runtime.Group): Unit //defined by macros

  //needed to go around the initialization order
  def postInit = {
    rounds.foreach(r => r.id = id) //set the id in round
  }

  protected def incrementRound: Unit //defined by macros

  protected def currentRound: Round //defined by macros

  final def send(): Set[(io.netty.buffer.ByteBuf,ProcessID)] = {
    incrementRound
    currentRound.packSend
  }

  final def update(msgs: Set[(io.netty.buffer.ByteBuf,ProcessID)]) {
    //TODO may set HO
    currentRound.unpackUpdate(msgs)
  }

  final def expectedNbrMessages: Int = currentRound.expectedNbrMessages

  //TODO when to insert dummy ?

  //////////////////////
  // for verification //
  //////////////////////

  //macros will take care of populating those fields
  val initState: round.formula.Formula
  val globalVariables: List[round.formula.Variable]
  val localVariables: List[round.formula.Variable]
  val ghostVariables: List[round.formula.Variable]
  val beforeProcessing: String
  val afterProcessing: String

}
