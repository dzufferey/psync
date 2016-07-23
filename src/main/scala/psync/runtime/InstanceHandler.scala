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

  @inline private final def more = again && !Thread.interrupted
  @inline private final def rndDiff(rnd: Int) = rnd - currentRound
  @inline private final def enoughMessages = received >= expected || !earlyMoving

  def run {
    Logger("InstanceHandler", Info, "starting instance " + instance)
    again = true
    var msg: DatagramPacket = null
    var msgRound = 0
    try {
      if (delayFirstSend > 0) {
        Thread.sleep(delayFirstSend)
      }
      // one round
      while(more) {
        // send the messages at the beginning of the round
        if (msg == null || sendWhenCatchingUp) {
          send
        }
        // accumulate messages
        var timedout = false
        while (rndDiff(msgRound) <= 0 &&    // not catching up
               !timedout &&                 // no TO yet
               !enoughMessages &&           // has not yet received enough messages
               more)                        // terminate
        {
          // try receive a new message
          if (msg == null) {
            msg = buffer.poll(timeout, TimeUnit.MILLISECONDS)
            // check that we have a message that we can handle
            if (msg != null) {
              didTimeOut -= 1
              val tag = Message.getTag(msg.content)
              if (instance != tag.instanceNbr) { // wrong instance
                msg.release
                msg = null
              } else if (tag.flag == Flags.normal) {
                // nothing to do we are fine
              } else if (tag.flag == Flags.dummy) {
                Logger("InstanceHandler", Debug, grp.self.id + ", " + instance + "dummy flag (ignoring)")
                msg.release
                msg = null
              } else {
                if (tag.flag == Flags.error) {
                  Logger("InstanceHandler", Warning, "error flag (pushing to user)")
                }
                default(msg)
                msg = null
              }
            } else {
              //Logger("InstanceHandler", Warning, instance + " timeout")
              didTimeOut += 1
              timedout = true
            }
            adaptTimeout
          }
          // process pending message
          if (msg != null) {
            msgRound = Message.getTag(msg.content).roundNbr
            val late = rndDiff(msgRound)
            if (late < 0) {
              // late message, ignore
              msg.release
              msg = null
            } else if (late == 0) {
              storePacket(msg)
              msg = null
            } // else we need to catch-up
          }
        }
        again &= deliver
        currentRound += 1
      }
    } catch {
      case _: java.lang.InterruptedException => ()
      case t: Throwable =>
        Logger("InstanceHandler", Error, "got an error " + t + " terminating instance: " + instance + "\n  " + t.getStackTrace.mkString("\n  "))
    } finally {
      if (msg != null) {
        msg.release
      }
      stop
    }
  }

  ///////////////////
  // current round //
  ///////////////////

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

  protected def deliver = {
    Logger("InstanceHandler", Debug, grp.self.id + ", " + instance + " delivering for round " + currentRound + " -> " + received)
    val toDeliver = messages.slice(0, received)
    val msgs = fromPkts(toDeliver)
    clear
    //push to the layer above
    //actual delivery
    proc.update(msgs)
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
    Logger("InstanceHandler", Debug,
      grp.self.id + ", " + instance + " sending for round " + currentRound + " -> " + sent + "\n" +
      grp.self.id + ", " + instance + " expected for round " + currentRound + " -> " + expected )
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
