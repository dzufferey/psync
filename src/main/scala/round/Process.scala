package round

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

  protected var allocator: io.netty.buffer.ByteBufAllocator = io.netty.buffer.PooledByteBufAllocator.DEFAULT

  final def send(): Set[(ProcessID, io.netty.buffer.ByteBuf)] = {
    incrementRound
    currentRound.packSend(allocator)
  }

  final def update(msgs: Set[(ProcessID, io.netty.buffer.ByteBuf)]) {
    currentRound.unpackUpdate(msgs)
  }

  final def expectedNbrMessages: Int = currentRound.expectedNbrMessages

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
