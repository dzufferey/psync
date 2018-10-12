package psync.runtime

import psync._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import io.netty.buffer.{ByteBuf,PooledByteBufAllocator}
import io.netty.channel.socket._
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.TimeUnit
import psync.utils.serialization.{KryoSerializer, KryoByteBufInput, KryoByteBufOutput}


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

  protected val sendWhenCatchingUp = options.sendWhenCatchingUp
  protected val delayFirstSend = options.delayFirstSend
  
  protected var timeout = options.timeout
  protected var roundStart: Long = 0
  protected var enoughMessages = false

  protected var instance: Short = 0
  protected var self: ProcessID = new ProcessID(-1)

  protected var n = 0
  protected var currentRound = 0

  protected var from: Array[Boolean] = null

  protected val globalSizeHint = options.packetSize
  protected val kryoIn = new KryoByteBufInput(null)
  protected val kryoOut = new KryoByteBufOutput(null)
  protected val kryo = {
    val k = KryoSerializer.serializer
    proc.registerSerializer(k)
    k
  }
  protected val allocator = PooledByteBufAllocator.DEFAULT

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

  @volatile protected var again = true

  def interrupt(inst: Int) {
    if (instance == inst)
      again = false
  }

  @inline private final def more = again && !Thread.interrupted
  @inline private final def rndDiff(rnd: Int) = rnd - currentRound
  private final val block = Long.MinValue

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
        initRound
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
            if (timeout == block) {
              msg = buffer.take()
            } else {
              val to = roundStart + timeout - java.lang.System.currentTimeMillis()
              if (to <= 0) {
                timedout = true
              } else {
                msg = buffer.poll(timeout, TimeUnit.MILLISECONDS)
              }
            }
            // check that we have a message that we can handle
            if (msg != null) {
              if (!checkInstanceAndTag(msg)) {
                msg = null
              }
            } else {
              //Logger("InstanceHandler", Warning, instance + " timeout")
              timedout = true
            }
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
              processPacket(msg)
              msg = null
            } // else we need to catch-up
          }
        }
        again &= update(timedout || rndDiff(msgRound) > 0) //consider catching up as TO
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

  protected def checkProgress(p: Progress, init: Boolean) {
    //TODO (non)strict
    if (p.isTimeout) {
      timeout = p.timeout
    } else if (p.isGoAhead) {
      enoughMessages = true
    } else if (p.isWaitMessage) {
      timeout = block
    } else if (p.isUnchanged) {
      if (init) {
        Logger.logAndThrow("InstanceHandler", Error, "Progress of init should not be Unchanged.")
      } else {
        // nothing to do I guess
      }
    } else {
      Logger.logAndThrow("InstanceHandler", Error, "Progress !?!?")
    }
  }

  protected def initRound {
    // clean
    enoughMessages = false
    for (i <- 0 until n) {
      from(i) = false
    }
    //
    roundStart = java.lang.System.currentTimeMillis()
    checkProgress(proc.init, true)
  }

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

  protected def processPacket(msg: Message) {
    val sender = msg.senderId
    if (!from(sender.id)) {
      from(sender.id) = true
      assert(msg.round == currentRound, msg.round + " vs " + currentRound)
      val buffer = msg.bufferAfterTag
      kryoIn.setBuffer(buffer)
      checkProgress(proc.receive(kryo, sender, kryoIn), false)
      kryoIn.setBuffer(null: ByteBuf)
    }
    msg.release
  }

  protected def update(didTimeout: Boolean) = {
    Logger("InstanceHandler", Debug, "Replica " + self.id + ", instance " + instance + " delivering for round " + currentRound + (if (didTimeout) " with TO" else ""))
    proc.update(didTimeout)
  }

  protected def send {
    val tag = Tag(instance, currentRound)
    var sent = 0
    var buffer: ByteBuf = null
    def alloc(sizeHint: Int): KryoByteBufOutput = {
      buffer = if (sizeHint > 0) allocator.buffer(sizeHint + tag.size)
               else if (globalSizeHint > 0) allocator.buffer(globalSizeHint + tag.size)
               else allocator.buffer()
      buffer.writeLong(tag.underlying)
      kryoOut.setBuffer(buffer)
      kryoOut
    }
    def sending(pid: ProcessID) {
      assert(pid != self)
      pktServ.send(pid, buffer)
      sent += 1
    }
    checkProgress(proc.send(kryo, alloc, sending), false)
    kryoOut.setBuffer(null: ByteBuf)
    Logger("InstanceHandler", Debug, "Replica " + self.id + ", instance " + instance + " sending for round " + currentRound + " -> " + sent + "\n")
  }
  
}
