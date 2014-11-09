package round.predicate

import round._
import round.formula._
import round.runtime._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import scala.reflect.ClassTag
import io.netty.buffer.ByteBuf
import io.netty.channel._
import io.netty.channel.socket._
import java.util.concurrent.locks.ReentrantLock

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
  
  protected val lock = new ReentrantLock
  
  //TODO interface between predicate and the algorithm: ...

  //what do predicates implement ?

  //general receive (not sure if it is the correct round).
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

  //register in the channel and send the first set of messages
  def start {
    dispatcher.add(instance, this)
    send
  }

  //things to do when changing round (overridden in sub classes)
  protected def atRoundChange { }
  protected def afterSend { }
  protected def afterUpdate { }
  
  protected def deliver {
    lock.lock
    try {
      //Logger("Predicate", Debug, "delivering for round " + currentRound + " (received = " + received + ")")
      val toDeliver = messages.slice(0, received)
      val msgs = fromPkts(toDeliver)
      currentRound += 1
      clear
      //push to the layer above
      try {
        //actual delivery
        val mset = msgs.toSet
        proc.update(mset)
        afterUpdate
        //start the next round (if has not exited)
        send
      } catch {
        case e: TerminateInstance =>
          stop
        case e: Throwable =>
          Logger("Predicate", Error, "got an error " + e + " terminating instance: " + instance)
          stop
          throw e
      }
    } finally {
      lock.unlock
    } 
  }
  
  //deregister
  def stop {
    lock.lock
    try {
      Logger("Predicate", Info, "stopping instance " + instance)
      dispatcher.remove(instance)
      var idx = 0
      while (idx < n) {
        if (messages(idx) != null) {
          messages(idx).release
          messages(idx) = null
        }
        idx += 1
      }
    } finally {
      lock.unlock
    }
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
    //Logger("Predicate", Debug, "sending for round " + currentRound)
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

  def messageReceived(pkt: DatagramPacket) = {
    val tag = Message.getTag(pkt.content)
    assert(instance == tag.instanceNbr)
    if (tag.flag == Flags.normal) {
      receive(pkt)
      true
    } else if (tag.flag == Flags.dummy) {
      Logger("Predicate", Debug, "messageReceived: dummy flag (ignoring)")
      pkt.release
      true
    } else if (tag.flag == Flags.error) {
      Logger("Predicate", Warning, "messageReceived: error flag (pushing to user)")
      false
    } else {
      //Logger("Predicate", Warning, "messageReceived: unknown flag -> " + tag.flag + " (ignoring)")
      false
    }
  }
    
  protected def toPkts(msgs: Seq[(ProcessID, ByteBuf)]): Seq[DatagramPacket] = {
    val src = grp.idToInet(grp.self)
    val tag = Tag(instance, currentRound)
    val pkts = msgs.map{ case (dst,buf) =>
      val dst2 = grp.idToInet(dst)
      buf.setLong(0, tag.underlying)
      new DatagramPacket(buf, dst2, src)
    }
    pkts
  }

  protected def fromPkts(pkts: Seq[DatagramPacket]): Seq[(ProcessID, ByteBuf)] = {
    val msgs = pkts.map( pkt => {
      val src = grp.inetToId(pkt.sender)
      val buf = pkt.content
      (src, buf)
    })
    msgs
  }


}
