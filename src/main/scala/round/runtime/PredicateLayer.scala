package round.runtime

import round._

import scala.reflect.ClassTag
import io.netty.channel._
import io.netty.channel.socket._

//basic implementation of rounds
//not so fault tolerant: ∀ r. ∃ p. |HO(p, r)| = n  ∧  ∀ p q. p sends a message to q at round r.

//first version that only uses the tag, just check the round# and the #msg received
//this assumes no network packet duplication
//TODO: put into the array at the ID to deal with duplication

class PredicateLayer(grp: Group, instance: Short) extends SimpleChannelInboundHandler[DatagramPacket](false) {

  val n = grp.replicas.length
  var currentRound = 0

  val messages = Array.ofDim[DatagramPacket](n)
  var received = 0 //Array.fill(n)(false)
  //var spill = new java.util.concurrent.ConcurrentLinkedQueue[DatagramPacket]()

  val lock = new scala.concurrent.Lock

  private def clear {
    received = 0
    for (i <- 0 until n)
      messages(i) = null
  }
  
  private def deliver {
    //TODO need to
    //- pair with id an put in a set
    //- clear (with a less aggressive sync this allows the next guy to proceed)
    //- get the round
    //- unpickle the messages (and release the buffer)
    //- push to the layer above
  }

  private def normalReceive(pkt: DatagramPacket) {
    messages(received) = pkt
    received += 1
    if (received == n) {
      deliver
    }
  }

  def receive(pkt: DatagramPacket) {
    val tag = Message.getTag(pkt.content)
    val round = tag.roundNbr
    lock.acquire //TODO less aggressive synchronization
    try {
      if(round == currentRound) {
        normalReceive(pkt)
      } else if (round > currentRound) {
        //we are late, need to catch up
        for (i <- currentRound until round) {
          deliver
        }
        //then back to normal
        normalReceive(pkt)
      } else {
        //late message, drop it
      }
    } finally {
      lock.release
    }
  }

  //in Netty version 5.0 will be called: channelRead0 will be messageReceived
  override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
    receive(pkt)
  }

}
