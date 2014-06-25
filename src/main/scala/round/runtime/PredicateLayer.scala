package round.runtime

import round._
import Algorithm._

import scala.reflect.ClassTag
import io.netty.buffer.ByteBuf
import io.netty.channel._
import io.netty.channel.socket._

//basic implementation of rounds
//not so fault tolerant: ∀ r. ∃ p. |HO(p, r)| = n  ∧  ∀ p q. p sends a message to q at round r.

//first version that only uses the tag, just check the round# and the #msg received
//this assumes no network packet duplication
//TODO: have an array of boolean indexeb by ID to deal with duplication

class PredicateLayer(
      grp: Group,
      val instance: Short,
      outChan: Channel,
      proc: ProcessWrapper
    ) extends SimpleChannelInboundHandler[DatagramPacket](false)
{

  val n = grp.size
  proc.setGroup(grp)
  var currentRound = 0

  val messages = Array.ofDim[DatagramPacket](n)
  var received = 0 //Array.fill(n)(false)
  //var spill = new java.util.concurrent.ConcurrentLinkedQueue[DatagramPacket]()

  val lock = new scala.concurrent.Lock


  def send {
    val myAddress = grp.idToInet(grp.self)
    val pkts = toPkts(proc.send)
    for (pkt <- pkts) {
      if (pkt.recipient() == myAddress) {
        normalReceive(pkt)
      } else {
        outChan.write(pkt, outChan.voidPromise())
      }
    }
    outChan.flush
  }


  protected def clear {
    received = 0
    for (i <- 0 until n)
      messages(i) = null
  }
  
  protected def deliver {
    val toDeliver = messages.slice(0, received)
    clear
    currentRound += 1
    //push to the layer above
    val msgs = fromPkts(toDeliver)
    proc.update(msgs)
    //start the next round (if has not exited)
    send
  }

  protected def normalReceive(pkt: DatagramPacket) {
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
      //TODO take round overflow into account
      if(round == currentRound) {
        normalReceive(pkt)
      } else if (round > currentRound) {
        //we are late, need to catch up
        for (i <- currentRound until round) {
          deliver //TODO skip the sending ?
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
    val tag = Message.getTag(pkt.content)
    if (instance == tag.instanceNbr) {
      receive(pkt)
    } else {
      ctx.fireChannelRead(pkt)
    }
  }
    
  protected def toPkts(msgs: Seq[(ByteBuf,ProcessID)]): Seq[DatagramPacket] = {
    val src = grp.idToInet(grp.self)
    val tag = Tag(instance, currentRound)
    val pkts = msgs.map{ case (buf,dst) =>
      val dst2 = grp.idToInet(dst)
      buf.setLong(0, tag.underlying)
      new DatagramPacket(buf, dst2, src)
    }
    pkts
  }

  protected def fromPkts(pkts: Seq[DatagramPacket]): Seq[(ByteBuf, ProcessID)] = {
    val msgs = pkts.map( pkt => {
      val src = grp.inetToId(pkt.sender)
      val buf = pkt.content
      (buf, src)
    })
    msgs
  }

}
