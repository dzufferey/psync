package round

import Algorithm._
import runtime.Group

import io.netty.buffer.ByteBuf

abstract class Process(val id: ProcessID) {

  //use some Macro to rewrite that and to type the round correctly
  //we should allow different rounds to have different types of messages
  type T
  val rounds: Array[Round[T]]

  //to finish the instance
  protected def exit() {
    sys.error("TODO")
  }

  //////////////////
  // util methods //
  //////////////////

  def setGroup(g: Group): Unit //defined by macros

  protected def incrementRound: Unit //defined by macros

  protected def currentRound: Round[T] //defined by macros

  final def send(): Set[(ByteBuf,ProcessID)] = {
    incrementRound
    currentRound.packSend
  }

  final def update(msgs: Set[(ByteBuf,ProcessID)]) {
    //TODO may set HO
    currentRound.unpackUpdate(msgs)
  }

  //TODO when to insert dummy ?


}
