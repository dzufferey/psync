package round.runtime

import round._
import io.netty.buffer.ByteBuf

class RunTime[IO](val alg: Algorithm[IO]) {

  //TODO something to keep track of variables provided by our system
  //-r: round number ...
  //-n: number of processes
  //-HO: heard-of

  def startInstance(
      instanceId: Short,
      io: IO,
      messages: Set[Message[ByteBuf]] = Set.empty)
  {
    sys.error("TODO")
  }

  def startService(defaultHandler: Message[ByteBuf] => Unit) {
    sys.error("TODO")
  }

  def shutdown {
    sys.error("TODO")
  }

}
