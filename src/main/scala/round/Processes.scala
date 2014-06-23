package round

import Algorithm._

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

  //TODO set n

  protected def incrementRound: Unit = {
    sys.error("have macro fill the gap ...")
  }

  private final def currentRound: Round[T] = {
    sys.error("have macro fill the gap ...")
    //rounds(r)
  }

  final def send(): Set[(ByteBuf,ProcessID)] = {
    //TODO increment r
    currentRound.packSend
  }

  final def update(msgs: Set[(ByteBuf,ProcessID)]) {
    //TODO may set HO
    currentRound.unpackUpdate(msgs)
  }

  //TODO connect with the rest
  //TODO when to insert dummy ?


}
