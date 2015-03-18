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
      channel: Channel,
      dispatcher: InstanceDispatcher,
      options: Map[String, String] = Map.empty
    )
{
  
  protected val lock = new ReentrantLock
  @volatile
  protected var active = false
  

  protected var pool: PredicatePool = null
  def setPool(p: PredicatePool) { pool = p }

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
    if (proc != null) {
      proc.reset
      proc = null
    }
    clear
  }

  protected def checkResources {
    if (messages == null || messages.size != n) {
      messages = Array.ofDim[DatagramPacket](n)
    }
  }

  //TODO interface between predicate and the algorithm: ...


  //general receive (not sure if it is the correct round, instance, etc.).
  def receive(pkt: DatagramPacket): Unit


  //register in the channel and send the first set of messages
  def start(g: Group, inst: Short, p: RtProcess, msgs: Set[Message]) {
    lock.lock
    try {
      grp = g
      instance = inst
      proc = p
      n = g.size
      currentRound = 0
      received = 0
      expected = n
      checkResources
      ////
      active = true
      dispatcher.add(instance, this)
      send
      msgs.foreach( p => receive(p.packet) )
    } finally {
      lock.unlock
    }
  }
  
  //deregister
  def stop(inst: Short) {
    lock.lock
    if (!active || instance != inst) {
     lock.unlock
    } else {
      try {
        active = false
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
        if (pool != null) {
          pool.recycle(this)
        }
      } finally {
        lock.unlock
      }
    }
  }

  protected def storePacket(pkt: DatagramPacket) {
    assert(lock.isHeldByCurrentThread, "lock.isHeldByCurrentThread")
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
  
  protected def deliver {
    assert(lock.isHeldByCurrentThread, "lock.isHeldByCurrentThread")
    Logger("Predicate", Debug, grp.self.id + ", " + instance + " delivering for round " + currentRound + " (received = " + received + ")")
    val toDeliver = messages.slice(0, received)
    val msgs = fromPkts(toDeliver)
    currentRound += 1
    clear
    //push to the layer above
    try {
      //actual delivery
      val mset = msgs.toSet
      if (proc.update(mset)) {
        //start the next round (if has not exited)
        send
      } else {
        stop(instance)
      }
    } catch {
      case e: TerminateInstance =>
        stop(instance)
      case e: Throwable =>
        Logger("Predicate", Error, "got an error " + e + " terminating instance: " + instance)
        stop(instance)
        throw e
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
