package psync.runtime

import psync._
import psync.ProcessID
import psync.runtime.server._
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
//-catch-up:
//  - eager: as soon as one message from the next round is received start catching up
//  - new round: after the current round finishes
//TODO break it into smaller parts
//-for learning TO/roundDuration
//  - used a discounted sum / geometric serie: coeff, window, expected RTT
//  - step increment/decrement
//  - fixed
trait InstHandler {

  /** Handle packets received from this instance */
  def newPacket(dp: DatagramPacket): Unit

  /** This instance should stop.
   *  Since there might be multiple threads working. It might take
   *  some time after this call returns until the instance actually
   *  finishes. */
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

  protected var from: Array[Boolean] = null
  protected var roundHasEnoughMessages = false


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

  /** Prepare the handler for a execution.
   *  call this just before giving it to the executor */
  def prepare(io: IO, g: Group, inst: Short, msgs: Set[Message]) {
    // clear the buffer
    freeRemainingMessages

    // init the process
    proc.setGroup(g)
    proc.init(io)

    // init this
    instance = inst
    grp = g
    n = g.size
    currentRound = 0

    // checkResources
    if (from == null || from.size != n) {
      from = Array.ofDim[Boolean](n)
      for (i <- 0 until n) from(i) = false
    }

    // enqueue pending messages
    msgs.foreach(p => newPacket(p.packet))

    // register
    dispatcher.add(inst, this)
  }

  protected def freeRemainingMessages {
    var pkt = buffer.poll
    while(pkt != null) {
      pkt.release
      pkt = buffer.poll
    }
    var i = 0
    while (i < n) {
      from(i) = false
      i += 1
    }
  }

  protected def stop {
    Logger("InstanceHandler", Info, "stopping instance " + instance)
    dispatcher.remove(instance)
    freeRemainingMessages
    proc.cleanUp
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
  @inline private final def enoughMessages = roundHasEnoughMessages || !earlyMoving

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
              if (!checkInstanceAndTag(msg)) {
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
              val sender = grp.inetToId(msg.sender)
              storePacket(sender, msg.content)
              msg = null
            } // else we need to catch-up
          }
        }
        again &= update
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

  // responsible for freeing the msg if returns false
  protected def checkInstanceAndTag(msg: DatagramPacket): Boolean = {
    val tag = Message.getTag(msg.content)
    if (instance != tag.instanceNbr) { // wrong instance
      msg.release
      false
    } else if (tag.flag == Flags.normal) {
      // nothing to do we are fine
      true
    } else if (tag.flag == Flags.dummy) {
      Logger("InstanceHandler", Debug, grp.self.id + ", " + instance + "dummy flag (ignoring)")
      msg.release
      false
    } else {
      if (tag.flag == Flags.error) {
        Logger("InstanceHandler", Warning, "error flag (pushing to user)")
      }
      default(msg)
      false
    }
  }

  protected def storePacket(sender: ProcessID, buf: ByteBuf) {
    val id = sender.id
    if (!from(id)) {
      from(id) = true
      assert(Message.getTag(buf).roundNbr == currentRound, Message.getTag(buf).roundNbr + " vs " + currentRound)
      roundHasEnoughMessages = proc.receive(sender, buf)
    } else {
      // duplicate packet
      buf.release
    }
  }

  protected def update = {
    Logger("InstanceHandler", Debug, grp.self.id + ", " + instance + " delivering for round " + currentRound)
    // clean
    roundHasEnoughMessages = false
    for (i <- 0 until n) {
      from(i) = false
    }
    // update
    proc.update
  }

  protected def send {
    val tag = Tag(instance, currentRound)
    var sent = 0
    def sending(pid: ProcessID, payload: ByteBuf) {
      payload.setLong(0, tag.underlying)
      if (pid == grp.self) {
        storePacket(pid, payload)
      } else {
        pktServ.send(pid, payload)
      }
      sent += 1
    }
    proc.send(sending)
    Logger("InstanceHandler", Debug,
      grp.self.id + ", " + instance + " sending for round " + currentRound + " -> " + sent + "\n")
  }
  
}
