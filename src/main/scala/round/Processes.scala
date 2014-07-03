package round

import Algorithm._
import runtime.Group

import io.netty.buffer.ByteBuf

abstract class Process(val id: ProcessID) {

  //use some Macro to rewrite that and to type the round correctly
  //we should allow different rounds to have different types of messages
  val rounds: Array[Round]

  //////////////////
  // util methods //
  //////////////////

  def setGroup(g: Group): Unit //defined by macros

  protected def incrementRound: Unit //defined by macros

  protected def currentRound: Round //defined by macros

  final def send(): Set[(ByteBuf,ProcessID)] = {
    incrementRound
    currentRound.packSend
  }

  final def update(msgs: Set[(ByteBuf,ProcessID)]) {
    //TODO may set HO
    currentRound.unpackUpdate(msgs)
  }

  //TODO when to insert dummy ?

  //////////////////////
  // for verification //
  //////////////////////

  //macros will take care of populating those fields
  val initState: round.formula.Formula
  val globalVariables: List[round.formula.Variable]
  val localVariables: List[round.formula.Variable]
  val ghostVariables: List[round.formula.Variable]

}
