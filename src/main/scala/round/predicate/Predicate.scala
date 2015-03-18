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

class Predicate(
      channel: Channel,
      dispatcher: InstanceDispatcher,
      options: Map[String, String] = Map.empty
    )
{
  
  protected var grp: Group = null
  protected var instance: Short = 0
  protected var proc: RtProcess = null

  protected var n = 0
  protected var currentRound = 0
  
  protected var expected = 0

  protected var messages: Array[DatagramPacket] = null
  protected var from: Array[Boolean] = null
  protected var received = 0

  def reset {
    grp = null
    proc = null
    var idx = 0
    while (idx < n) {
      if (messages(idx) != null) {
        messages(idx).release
        messages(idx) = null
      }
      idx += 1
    }
    for (i <- 0 until n) from(i) = false
  }

  protected def checkResources {
    if (messages == null || messages.size != n) {
      messages = Array.ofDim[DatagramPacket](n)
      for (i <- 0 until n) messages(i) = null
    }
    if (from == null || from.size != n) {
      from = Array.ofDim[Boolean](n)
      for (i <- 0 until n) from(i) = false
    }
  }

  //TODO interface between predicate and the algorithm: ...


  //general receive (not sure if it is the correct round, instance, etc.).
  def receive(pkt: DatagramPacket) {
    val tag = Message.getTag(pkt.content)
    val round = tag.roundNbr
    if (instance != tag.instanceNbr) {
      pkt.release
    } else {
      try {
        while(round - currentRound > 0) {
          //println(grp.self.id + ", " + tag.instanceNbr + " catching up: " + currentRound + " -> " + round)
          deliver
        }
      } catch {
        case t: Throwable =>
          pkt.release
          throw t
      }
      if (round == currentRound) {
        //println(grp.self.id + ", " + tag.instanceNbr + " delivering: " + currentRound)
        //normal case
        storePacket(pkt)
        if (received >= expected) {
          deliver
        }
      } else {
        pkt.release //packet late
      }
    }
  }


  def init(g: Group, inst: Short, p: RtProcess) {
    grp = g
    instance = inst
    proc = p
    n = g.size
    currentRound = 0
    received = 0
    expected = n
    checkResources
  }

  def stop {
    //TODO better
    throw new TerminateInstance
  }

  protected def storePacket(pkt: DatagramPacket) {
    val id = grp.inetToId(pkt.sender).id
    if (!from(id)) {
      from(id) = true
      messages(received) = pkt
      received += 1
      assert(Message.getTag(pkt.content).roundNbr == currentRound, Message.getTag(pkt.content).roundNbr + " vs " + currentRound)
    } else {
      pkt.release
    }
  }
  
  def deliver {
    Logger("Predicate", Debug, grp.self.id + ", " + instance + " delivering for round " + currentRound + " (received = " + received + ")")
    val toDeliver = messages.slice(0, received)
    val msgs = fromPkts(toDeliver)
    currentRound += 1
    clear
    //push to the layer above
    //actual delivery
    val mset = msgs.toSet
    if (proc.update(mset)) {
      //start the next round (if has not exited)
      send
    } else {
      stop
    }
  }
  
  protected def clear {
    val r = received
    received = 0
    for (i <- 0 until r) {
      messages(i) = null
    }
    for (i <- 0 until n) {
      from(i) = false
    }
  }

  ////////////////
  // utils + IO //
  ////////////////
  
  def send {
    //Logger("Predicate", Debug, "sending for round " + currentRound)
    val myAddress = grp.idToInet(grp.self)
    val pkts = toPkts(proc.send.toSeq)
    expected = proc.expectedNbrMessages
    //println(grp.self.id + ", " + instance + " round: " + currentRound + ", expected " + expected)
    for (pkt <- pkts) {
      if (pkt.recipient() == myAddress) {
        storePacket(pkt)
      } else {
        channel.write(pkt, channel.voidPromise())
      }
    }
    channel.flush
    if (received >= expected) {
      deliver
    }
  }

  def messageReceived(pkt: DatagramPacket) = {
    val tag = Message.getTag(pkt.content)
    if (instance != tag.instanceNbr) {
      pkt.release
      true
    } else if (tag.flag == Flags.normal) {
      receive(pkt)
      true
    } else if (tag.flag == Flags.dummy) {
      Logger("Predicate", Debug, grp.self.id + ", " + instance + " messageReceived: dummy flag (ignoring)")
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
      val dst2 = grp.idToInet(dst, instance)
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
