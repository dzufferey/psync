package round.runtime

import round._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import io.netty.channel.Channel
import io.netty.buffer.ByteBuf
import io.netty.channel.socket._
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.TimeUnit


//TODO what should be the interface ?
//- val lock = new java.util.concurrent.locks.ReentrantLock
//- @volatile var roundStart: Long
//- var roundDuration: Long //TO is roundStart+roundDuration
//- def newPacket(dp: DatagramPacket): Unit
//- def interrupt(inst: Short): Unit or stop(inst: Short)
//TODO options
//-TO:
//  - non-strict: proceed as soon as there is enough messages
//  - strict: wait until TO to proceed
//-catch-up:
//  - eager: as soon as one message from the next round is received start catching up
//  - new round: after the current round finishes
//TODO break it into smaller parts
//-for learning TO/roundDuration
//  - used a discounted sum / geometric serie: coeff, window, expected RTT
//  - step increment/decrement
//  - fixed
//TODO if making a TCP RT we cannot use DatagramPacket anymore ?
trait InstHandler {
  def newPacket(dp: DatagramPacket): Unit
  def interrupt(inst: Int): Unit
}

class InstanceHandler[IO](proc: Process[IO],
                          rt: round.runtime.RunTime[IO],
                          channel: Channel,
                          dispatcher: InstanceDispatcher,
                          defaultHandler: DatagramPacket => Unit,
                          options: Map[String, String] = Map.empty) extends Runnable with InstHandler {
  
  protected val defaultBufferSize = 16
  protected val bufferSize = try options.getOrElse("bufferSize", defaultBufferSize.toString).toInt
                             catch { case _: Throwable => defaultBufferSize } //TODO some logging
  protected val buffer = new ArrayBlockingQueue[DatagramPacket](bufferSize) 
  
  protected var timeout = {
    try options.getOrElse("timeout", "20").toInt
    catch {
      case e: Exception =>
        Logger("InstanceHandler", Warning, "timeout unspecified or wrong format, using 20")
        20 //milliseconds
    }
  }
  
  protected val adaptative = {
    try options.getOrElse("adaptative", "false").toBoolean
    catch {
      case e: Exception =>
        Logger("Predicate", Warning, "adaptative has wrong format, reverting to false.")
        false
    }
  }
  
  protected var didTimeOut = 0

  protected var instance: Short = 0
  protected var grp: Group = null

  protected var n = 0
  protected var currentRound = 0
  protected var expected = 0

  protected var messages: Array[DatagramPacket] = null
  protected var from: Array[Boolean] = null
  protected var received = 0
  

  /** A new packet is received and should be processed */
  def newPacket(dp: DatagramPacket) = {
    if (!buffer.offer(dp)) {
      Logger("InstanceHandler", Warning, "too many packets")
      dp.release
    }
  }
  
  /** Forward the packet to the defaultHandler */
  protected def default(pkt: DatagramPacket) {
    rt.submitTask(new Runnable { def run = defaultHandler(pkt) })
  }
  
  /** Allocate new buffers if needed */
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

  /** Prepare the handler for a execution.
   *  call this just before giving it to the executor */
  def prepare(io: IO, g: Group, inst: Short, msgs: Set[Message]) {
    //clear the buffer
    var pkt = buffer.poll
    while(pkt != null) {
      pkt.release
      pkt = buffer.poll
    }
    freeRemainingMessages

    instance = inst
    proc.setGroup(g)
    proc.init(io)

    grp = g
    n = g.size
    currentRound = 0
    received = 0
    expected = n
    checkResources

    msgs.foreach(p => newPacket(p.packet))
    dispatcher.add(inst, this)
  }
  
  protected def freeRemainingMessages {
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

  protected def stop {
    Logger("InstanceHandler", Info, "stopping instance " + instance)
    dispatcher.remove(instance)
    freeRemainingMessages
    rt.recycle(this)
  }

  protected var again = true

  def interrupt(inst: Int) {
    if (instance == inst)
      again = false
  }

  def run {
    try {
      Logger("InstanceHandler", Info, "starting instance " + instance)
      again = true
      send //TODO might already stop here
      while(again && !Thread.interrupted()) {
        val msg = buffer.poll(timeout, TimeUnit.MILLISECONDS)
        if (msg != null) {
          didTimeOut -= 1
          //TODO this should return: ok, not received, or stopping
          if(!messageReceived(msg))
            default(msg)
        } else {
          //Logger("InstanceHandler", Warning, instance + " timeout")
          didTimeOut += 1
          deliver
        }
        if (adaptative) {
          //TODO something amortized to avoid oscillations
          if (didTimeOut > 5) {
            didTimeOut = 0
            timeout += 10
          } else if (didTimeOut < -50) {
            didTimeOut = 0
            timeout -= 10
          }
        }
      }
    } catch {
      case _: TerminateInstance | _: java.lang.InterruptedException => ()
      case t: Throwable =>
        Logger("InstanceHandler", Error, "got an error " + t + " terminating instance: " + instance + "\n  " + t.getStackTrace.mkString("\n  "))
    } finally {
      stop
    }
  }
  
  ///////////////////
  // current round //
  ///////////////////
  
  //general receive (not sure if it is the correct round, instance, etc.).
  protected def receive(pkt: DatagramPacket) {
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
  
  protected def deliver {
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
      //TODO better!!
      throw new TerminateInstance
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
  
  protected def send {
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

  protected def messageReceived(pkt: DatagramPacket) = {
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
