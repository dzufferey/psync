package psync.runtime

import psync._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
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

class InstanceHandler[IO,P <: Process[IO]](proc: P,
                          rt: psync.runtime.Runtime[IO,P],
                          pktServ: PacketServer,
                          dispatcher: InstanceDispatcher,
                          defaultHandler: DatagramPacket => Unit,
                          options: RuntimeOptions) extends Runnable with InstHandler {

  protected val buffer = new ArrayBlockingQueue[DatagramPacket](options.bufferSize)

  protected var timeout = options.timeout
  protected val earlyMoving = options.earlyMoving
  protected val adaptative = options.adaptative
  protected val sendWhenCatchingUp = options.sendWhenCatchingUp
  protected val delayFirstSend = options.delayFirstSend

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

  @inline
  private def adaptTimeout {
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

  def run {
    try {
      Logger("InstanceHandler", Info, "starting instance " + instance)
      again = true
      if (delayFirstSend > 0) {
        Thread.sleep(delayFirstSend)
      }
      send //TODO might already stop here
      while(again && !Thread.interrupted()) {
        //TODO the timeout should also depends on when the round started!
        val msg = buffer.poll(timeout, TimeUnit.MILLISECONDS)
        if (msg != null) {
          didTimeOut -= 1
          //TODO this should return: ok, not received, or stopping
          if(!messageReceived(msg))
            default(msg)
        } else {
          //Logger("InstanceHandler", Warning, instance + " timeout")
          didTimeOut += 1
          deliver(false)
        }
        adaptTimeout
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

  //general receive (not sure if it is the correct round, but instance is correct).
  protected def receive(pkt: DatagramPacket) {
    val tag = Message.getTag(pkt.content)
    val round = tag.roundNbr
    try {
      while(round - currentRound > 0) {
        //println(grp.self.id + ", " + tag.instanceNbr + " catching up: " + currentRound + " -> " + round)
        if (round - currentRound == 1) {
          deliver(false)
        } else {
          deliver(true)
        }
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
      if (received >= expected && earlyMoving) {
        deliver(false)
      }
    } else {
      pkt.release //packet late
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

  protected def deliver(catchingUp: Boolean) {
    Logger("InstanceHandler", Debug, grp.self.id + ", " + instance + " delivering for round " + currentRound + " -> " + received)
    val toDeliver = messages.slice(0, received)
    val msgs = fromPkts(toDeliver)
    currentRound += 1
    clear
    //push to the layer above
    //actual delivery
    if (proc.update(msgs)) {
      //start the next round (if has not exited)
      if (!catchingUp || sendWhenCatchingUp) {
        send
      }
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
    val myAddress = grp.idToInet(grp.self)
    val pkts = toPkts(proc.send)
    var sent = 0
    expected = proc.expectedNbrMessages
    for (pkt <- pkts) {
      if (pkt.recipient() == myAddress) {
        storePacket(pkt)
      } else {
        pktServ.send(pkt)
      }
      sent += 1
    }
    Logger("InstanceHandler", Debug, grp.self.id + ", " + instance + " sending for round " + currentRound + " -> " + sent)
    Logger("InstanceHandler", Debug, grp.self.id + ", " + instance + " expected for round " + currentRound + " -> " + expected)
    if (received >= expected && earlyMoving) {
      deliver(false)
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

  protected def toPkts(msgs: Map[ProcessID, ByteBuf]): Iterable[DatagramPacket] = {
    val src = grp.idToInet(grp.self)
    val tag = Tag(instance, currentRound)
    val pkts = msgs.map{ case (dst,buf) =>
      val dst2 = grp.idToInet(dst, instance)
      buf.setLong(0, tag.underlying)
      new DatagramPacket(buf, dst2, src)
    }
    pkts
  }

  protected def fromPkts(pkts: Seq[DatagramPacket]): Map[ProcessID, ByteBuf] = {
    val msgs = pkts.foldLeft(Map.empty[ProcessID, ByteBuf])( (acc, pkt) => {
      val src = grp.inetToId(pkt.sender)
      val buf = pkt.content
      acc + (src -> buf)
    })
    msgs
  }

}
