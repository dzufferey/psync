package round.predicate

import round._
import Algorithm._
import round.runtime._

import scala.reflect.ClassTag
import io.netty.buffer.ByteBuf
import io.netty.channel._
import io.netty.channel.socket._

abstract class Predicate(
      grp: Group,
      val instance: Short,
      channel: Channel,
      proc: ProcessWrapper //TODO that process wrapper stuff is ugly and not necessary
    ) extends SimpleChannelInboundHandler[DatagramPacket](false)
{

  //TODO interface between predicate and the algorithm: ...

  //TODO what does it guarantee
  //TODO how to pick the predicate (when starting consensus ?)

  //what predicate to implement ?
  //-...
  //-...

}
