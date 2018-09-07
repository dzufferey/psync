package psync.runtime

import psync._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import io.netty.buffer.{ByteBuf,PooledByteBufAllocator}
import io.netty.channel.socket._
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.TimeUnit
import psync.utils.serialization.{KryoByteBufInput, KryoByteBufOutput}


//TODO what should be the interface ?
//- val lock = new java.util.concurrent.locks.ReentrantLock
//- @volatile var roundStart: Long
//- var roundDuration: Long //TO is roundStart+roundDuration
//- def newPacket(dp: DatagramPacket): Unit
//- def interrupt(inst: Short): Unit or stop(inst: Short)
//TODO break it into smaller parts
//-for learning TO/roundDuration
//  - used a discounted sum / geometric serie: coeff, window, expected RTT
//  - step increment/decrement
//  - fixed
trait InstHandler {

  /** Handle packets received from this instance */
  def newPacket(msg: Message): Unit

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
                          defaultHandler: Message => Unit,
                          options: RuntimeOptions) extends Runnable with InstHandler {

  protected val buffer = new ArrayBlockingQueue[Message](options.bufferSize)

  protected var timeout = options.timeout
  protected val earlyMoving = options.earlyMoving
  protected val adaptative = options.adaptative
  protected val sendWhenCatchingUp = options.sendWhenCatchingUp
  protected val delayFirstSend = options.delayFirstSend

  protected var didTimeOut = 0

  protected var instance: Short = 0
  protected var self: ProcessID = new ProcessID(-1)

  protected var n = 0
  protected var currentRound = 0

  protected var from: Array[Boolean] = null
  protected var roundHasEnoughMessages = false

  protected val globalSizeHint = options.packetSize
  protected val kryoIn = new KryoByteBufInput(null) //TODO could be shared further
  protected val kryoOut = new KryoByteBufOutput(null) //TODO could be shared further

  /** A new packet is received and should be processed */
  def newPacket(msg: Message) = {
    if (!buffer.offer(msg)) {
      Logger("InstanceHandler", Warning, "Replica " + self.id + " too many packets for instance " + instance)
      msg.release
    }
  }

  /** Forward the packet to the defaultHandler in another thread/task */
  protected def default(msg: Message) {
    rt.submitTask(new Runnable { def run = defaultHandler(msg) })
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
    self = g.self
    n = g.size
    currentRound = 0

    // checkResources
    if (from == null || from.size != n) {
      from = Array.ofDim[Boolean](n)
      for (i <- 0 until n) from(i) = false
    }

    // enqueue pending messages
    msgs.foreach(newPacket)

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
    var msg: Message = null
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
            msgRound = msg.round
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
  protected def checkInstanceAndTag(msg: Message): Boolean = {
    val tag = msg.tag
    if (instance != tag.instanceNbr) { // wrong instance
      msg.release
      false
    } else if (tag.flag == Flags.normal) {
      // nothing to do we are fine
      true
    } else if (tag.flag == Flags.dummy) {
      Logger("InstanceHandler", Debug, self.id + ", " + instance + "dummy flag (ignoring)")
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

  protected def storePacket(msg: Message) {
    val sender = msg.senderId
    if (!from(sender.id)) {
      from(sender.id) = true
      assert(msg.round == currentRound, msg.round + " vs " + currentRound)
      val buffer = msg.bufferAfterTag
      buffer.retain
      msg.release
      kryoIn.setBuffer(buffer)
      roundHasEnoughMessages = proc.receive(sender, kryoIn)
      kryoIn.setBuffer(null: ByteBuf)
      buffer.release
    } else {
      // duplicate packet
      msg.release
    }
  }

  protected def update = {
    Logger("InstanceHandler", Debug, "Replica " + self.id + ", instance " + instance + " delivering for round " + currentRound)
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
    def alloc(sizeHint: Int): KryoByteBufOutput = {
      val buffer = if (sizeHint > tag.size) PooledByteBufAllocator.DEFAULT.buffer(sizeHint)
                   else if (globalSizeHint > tag.size) PooledByteBufAllocator.DEFAULT.buffer(globalSizeHint)
                   else PooledByteBufAllocator.DEFAULT.buffer()
      buffer.writeLong(tag.underlying)
      kryoOut.setBuffer(buffer)
      kryoOut
    }
    def sending(pid: ProcessID, payload: KryoByteBufOutput) {
      val buffer = payload.getBBuffer
      payload.setBuffer(null: ByteBuf)
      assert(pid != self)
      pktServ.send(pid, buffer)
      sent += 1
    }
    proc.send(alloc, sending)
    Logger("InstanceHandler", Debug, "Replica " + self.id + ", instance " + instance + " sending for round " + currentRound + " -> " + sent + "\n")
  }
  
}
