package round.runtime

import round._

import scala.reflect.ClassTag
import io.netty.channel._
import io.netty.channel.socket._

//TODO implementation of rounds
//start with perfect rounds, then move the the real thing

//TODO extract round out of the Algorithm, and put additional data like process to replica mapping ...

//TODO rather than DatagramPacket, there should be something more abstract (that depends on A)

class PredicateLayer[A: ClassTag](self: Replica, peers: List[Replica]) extends SimpleChannelInboundHandler[DatagramPacket] {

  val n = peers.length+1
  val messages = Array.ofDim[A](n)
  val received = Array.fill(n)(false)

  def clear {
    for (i <- 0 until n)
      received(i) = false
  }
  
  //TODO override the method to filter packets

  //in Netty version 5.0 will be called: channelRead0 will be messageReceived
  override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
    println(pkt)
  }

}
