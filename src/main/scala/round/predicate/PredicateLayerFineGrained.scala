package round.predicate

import round._
import Algorithm._
import round.runtime._
import round.utils.Timer

import scala.reflect.ClassTag
import io.netty.buffer.ByteBuf
import io.netty.channel._
import io.netty.channel.socket._
import io.netty.util.{TimerTask, Timeout}

import round.utils.Logger
import round.utils.LogLevel._
  
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.Semaphore

//basic implementation of rounds
//not so fault tolerant: ∀ r. ∃ p. |HO(p, r)| = n  ∧  ∀ p q. p sends a message to q at round r.

class PredicateLayerFineGrained(
      grp: Group,
      val instance: Short,
      channel: Channel,
      dispatcher: InstanceDispatcher,
      proc: Process,
      options: Map[String, String] = Map.empty
    ) extends Predicate
{

  //safety condition guaranteed by the predicate
  val ensures = round.formula.True() 

  val n = grp.size
  proc.setGroup(grp)
  var currentRound = 0

  val messages = Array.ofDim[DatagramPacket](n)
  val from = Array.ofDim[AtomicBoolean](n)
  for (i <- 0 until n) from(i) = new AtomicBoolean(false)
  var received = new AtomicInteger(0)
  //var spill = new java.util.concurrent.ConcurrentLinkedQueue[DatagramPacket]()

  val maxPermits = 1000
  val lock = new Semaphore(1000)

  //register in the channel
  dispatcher.add(instance, this)

  //dealing with the timeout ?
  val defaultTO = {
    try {
      options.getOrElse("timeout", "200").toInt
    } catch {
      case e: Exception =>
        Logger("Predicate", Warning, "timeout unspecified or wrong format, using 200")
        200 //milliseconds
    }
  }

  //some flag about being active
  @volatile
  var active = true

  //each modification should set this to true, the timer will reset it
  @volatile
  var changed = false

  val tt = new TimerTask {
    def run(to: Timeout) {
      if (active) {
        if (changed) {
          changed = false
        } else {
          Logger("Predicate", Debug, "delivering because of timeout")
          deliver
          changed = false
        }
        timeout = Timer.newTimeout(this, defaultTO)
      }
    }
  }
  var timeout: Timeout = Timer.newTimeout(tt, defaultTO)


  //deregister
  def stop {
    active = false
    dispatcher.remove(instance)
    timeout.cancel
    Logger("Predicate", Info, "stopping instance " + instance)
  }

  def send {
    val myAddress = grp.idToInet(grp.self)
    val pkts = toPkts(proc.send.toSeq)
    for (pkt <- pkts) {
      if (pkt.recipient() == myAddress) {
        normalReceive(pkt)
      } else {
        channel.write(pkt, channel.voidPromise())
      }
    }
    channel.flush
  }


  protected def clear {
    received.set(0)
    for (i <- 0 until n) {
      messages(i) = null
      from(i).set(false)
    }
  }
  
  //assume the thread has one permit
  protected def deliver {
    lock.release //need to release to avoid deadlock
    lock.acquire(maxPermits)
    //need to test again the delivery condition
    if (received.get >= n) {
      val toDeliver = messages.slice(0, received.intValue)
      clear
      currentRound += 1
      //push to the layer above
      val msgs = fromPkts(toDeliver)
      try {
        proc.update(msgs.toSet)
        //start the next round (if has not exited)
        send
      } catch {
        case e: TerminateInstance => stop
      }
    }
    lock.release(maxPermits -1)
  }

  protected def normalReceive(pkt: DatagramPacket) {
    val id = grp.inetToId(pkt.sender)
    //protect from duplicate packet
    if (!from(id).getAndSet(true)) {
      val r = received.getAndIncrement()
      messages(r) = pkt
      if (r >= n) {
        deliver
      }
    }
    changed = true
  }

  def receive(pkt: DatagramPacket) {
    val tag = Message.getTag(pkt.content)
    val round = tag.roundNbr
    lock.acquire
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
