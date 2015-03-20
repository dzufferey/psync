package round.runtime

import round._
import round.predicate.Predicate
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import io.netty.channel.Channel
import io.netty.buffer.ByteBuf
import io.netty.channel.socket._
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.TimeUnit

//TODO extends a version without type param

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

  protected var instance = 0

  protected val pred = new Predicate(channel, dispatcher, options)
  
  protected val defaultBufferSize = 16
  protected val bufferSize = try options.getOrElse("bufferSize", defaultBufferSize.toString).toInt
                             catch { case _: Throwable => defaultBufferSize } //TODO some logging
  protected val buffer = new ArrayBlockingQueue[DatagramPacket](bufferSize) 
  
  protected var timeout = {
    try {
      options.getOrElse("timeout", "20").toInt
    } catch {
      case e: Exception =>
        Logger("InstanceHandler", Warning, "timeout unspecified or wrong format, using 20")
        20 //milliseconds
    }
  }
  
  protected val adaptative = {
    try {
      options.getOrElse("adaptative", "false").toBoolean
    } catch {
      case e: Exception =>
        Logger("Predicate", Warning, "adaptative has wrong format, reverting to false.")
        false
    }
  }
  
  protected var didTimeOut = 0

  def newPacket(dp: DatagramPacket) = {
    if (!buffer.offer(dp)) {
      Logger("InstanceHandler", Warning, "too many packets")
      dp.release
    }
  }
  
  protected def default(pkt: DatagramPacket) {
    rt.submitTask(new Runnable { def run = defaultHandler(pkt) })
  }

  //call this just before giving it to the executor
  def prepare(io: IO, g: Group, inst: Short, msgs: Set[Message]) {
    //clear the buffer
    var pkt = buffer.poll
    while(pkt != null) {
      pkt.release
      pkt = buffer.poll
    }

    instance = inst
    proc.setGroup(g)
    proc.init(io)
    pred.init(g, inst, proc)
    msgs.foreach(p => newPacket(p.packet))
    dispatcher.add(inst, this)
  }

  protected def stop {
    Logger("InstanceHandler", Info, "stopping instance " + instance)
    dispatcher.remove(instance)
    pred.reset
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
      pred.send //TODO might already stop here
      while(again && !Thread.interrupted()) {
        val msg = buffer.poll(timeout, TimeUnit.MILLISECONDS)
        if (msg != null) {
          didTimeOut -= 1
          //TODO this should return: ok, not received, or stopping
          if(!pred.messageReceived(msg))
            default(msg)
        } else {
          //Logger("InstanceHandler", Warning, instance + " timeout")
          didTimeOut += 1
          pred.deliver
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

}
