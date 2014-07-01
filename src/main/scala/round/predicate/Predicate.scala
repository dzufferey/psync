package round.predicate

import round._
import round.formula._
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
      proc: Process
    ) extends SimpleChannelInboundHandler[DatagramPacket](false)
{

  //TODO what does it guarantee
  val ensures: Formula

  //TODO interface between predicate and the algorithm: ...

  //what predicate to implement ?
  //-...
  //-...

}
