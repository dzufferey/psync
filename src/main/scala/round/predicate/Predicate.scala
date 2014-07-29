package round.predicate

import round._
import round.formula._
import Algorithm._
import round.runtime._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import scala.reflect.ClassTag
import io.netty.buffer.ByteBuf
import io.netty.channel._
import io.netty.channel.socket._

abstract class Predicate(
      val grp: Group,
      val instance: Short,
      channel: Channel,
      dispatcher: InstanceDispatcher,
      proc: Process,
      options: Map[String, String] = Map.empty
    )
{

  //TODO the expected # of msg

  //what does it guarantee
  val ensures: Formula
  
  //TODO interface between predicate and the algorithm: ...

  //what do predicates implement ?

  //general receive (not sure if it is the currect round).
  //vanilla implementation looks like:
  //  val round = Message.getTag(pkt.content).roundNbr
  //  if (round >= currentRound) {
  //    //we are late, need to catch up
  //    while(currentRound < round) { deliver }
  //    normalReceive(pkt)
  //  } // else: this is a late message, drop it
  def receive(pkt: DatagramPacket): Unit

  //for messages that we know already belong to the current round.
  //vanilla implementation looks like:
  //  val id = grp.inetToId(pkt.sender)
  //  messages(received) = pkt
  //  received += 1
  //  if (received >= expectedNbrMessage) {
  //    deliver
  //  }
  protected def normalReceive(pkt: DatagramPacket)


  val n = grp.size
  var currentRound = 0
  
  val messages = Array.ofDim[DatagramPacket](n)
  def received: Int
  def resetReceived: Unit

  //register in the channel
  def start {
    dispatcher.add(instance, this)
  }

  //things to do when changing round (overridden in sub classes)
  protected def atRoundChange { }
  protected def afterSend { }
  protected def afterUpdate { }
  
  protected def deliver {
    Logger("Predicate", Debug, "delivering for round " + currentRound + " (received = " + received + ")")
    val toDeliver = messages.slice(0, received)
    clear
    currentRound += 1
    //push to the layer above
    val msgs = fromPkts(toDeliver)
    try {
      //actual delivery
      proc.update(msgs.toSet)
      afterUpdate
      //start the next round (if has not exited)
      send
    } catch {
      case e: TerminateInstance => stop
    }
  }
  
  //deregister
  def stop {
    dispatcher.remove(instance)
    Logger("Predicate", Info, "stopping instance " + instance)
  }

  protected def clear {
    val r = received
    resetReceived
    for (i <- 0 until r) {
      messages(i) = null
    }
  }

  ////////////////
  // utils + IO //
  ////////////////
  
  def send {
    Logger("Predicate", Debug, "sending for round " + currentRound)
    val myAddress = grp.idToInet(grp.self)
    val pkts = toPkts(proc.send.toSeq)
    atRoundChange
    for (pkt <- pkts) {
      if (pkt.recipient() == myAddress) {
        normalReceive(pkt)
      } else {
        channel.write(pkt, channel.voidPromise())
      }
    }
    channel.flush
    afterSend
  }

  def messageReceived(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
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
