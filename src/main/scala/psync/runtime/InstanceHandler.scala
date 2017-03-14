package psync.runtime

import psync._
import psync.runtime.synchronizer._
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
                          pktSrv: PacketServer,
                          options: RuntimeOptions) extends Runnable with InstHandler {

  protected val buffer = new ArrayBlockingQueue[DatagramPacket](options.bufferSize)

  protected var instance: Short = 0
  protected val synchronizer = new BenignSynchonizer(proc, pktSrv, rt, options)
  protected var to = new TO(0)


  /** A new packet is received and should be processed */
  def newPacket(dp: DatagramPacket) = {
    if (!buffer.offer(dp)) {
      Logger("InstanceHandler", Warning, "too many packets")
      dp.release
    }
  }

  /** Prepare the handler for a execution.
   *  call this just before giving it to the executor */
  def prepare(io: IO, g: Group, inst: Short, msgs: Set[Message]) {
    // clear the buffer
    freeRemainingMessages

    instance = inst

    to = synchronizer.prepare(io, g, inst)
    assert(!to.isTerminated)

    // enqueue pending messages
    msgs.foreach(p => newPacket(p.packet))

    // register
    pktSrv.addToDispatch(inst, this)
  }

  protected def freeRemainingMessages {
    var pkt = buffer.poll
    while(pkt != null) {
      pkt.release
      pkt = buffer.poll
    }
  }

  protected def stop {
    Logger("InstanceHandler", Info, "stopping instance " + instance)
    pktSrv.removeFromDispatch(instance)
    freeRemainingMessages
    val ok = synchronizer.interrupt(instance)
    assert(ok)
    rt.recycle(this)
  }

  protected var again = true

  def interrupt(inst: Int) {
    if (instance == inst)
      again = false
  }

  @inline private final def more = again && !Thread.interrupted

  def run {
    Logger("InstanceHandler", Info, "starting instance " + instance)
    again = true
    try {
      // delay first send
      if (to.nextTimeout > 0) {
        Thread.sleep(to.nextTimeout)
      }
      to = synchronizer.start
      assert(to.isNextTimeout)
      var expire = java.lang.System.currentTimeMillis() + to.nextTimeout

      while(more) {
        val delta = expire - java.lang.System.currentTimeMillis()
        Logger("InstanceHandler", Debug, "delta = " + delta)
        if (delta > 0) {
          val msg = buffer.poll(delta, TimeUnit.MILLISECONDS)
          if (msg != null) {
            to = synchronizer.message(msg)
          } else {
            to = synchronizer.timeout
          }
        } else {
          to = synchronizer.timeout
        }
        if (to.isTerminated) {
          again = false
        } else if (to.isNextTimeout) {
          expire = java.lang.System.currentTimeMillis() + to.nextTimeout
        } // else is nothing
      }
    } catch {
      case _: java.lang.InterruptedException => ()
      case t: Throwable =>
        Logger("InstanceHandler", Error, "got an error " + t + " terminating instance: " + instance + "\n  " + t.getStackTrace.mkString("\n  "))
    } finally {
      stop
    }
  }

}
