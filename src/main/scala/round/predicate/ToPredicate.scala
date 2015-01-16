package round.predicate

import round._
import round.runtime._
import round.utils.Timer

import io.netty.buffer.ByteBuf
import io.netty.channel._
import io.netty.channel.socket._
import io.netty.util.{TimerTask, Timeout}

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import java.util.concurrent.locks.ReentrantLock


/* A predicate using timeout to deliver (when not all msg are received) */
class ToPredicate(
      channel: Channel,
      dispatcher: InstanceDispatcher,
      options: Map[String, String] = Map.empty
    ) extends Predicate(channel, dispatcher, options)
{

  override def checkResources {
    super.checkResources
    if (from == null || from.size != n) {
      from = Array.fill(n)(false)
    }
  }

  //dealing with the timeout ?
  protected var defaultTO = {
    try {
      options.getOrElse("timeout", "20").toInt
    } catch {
      case e: Exception =>
        Logger("Predicate", Warning, "timeout unspecified or wrong format, using 20")
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

  //each modification should set this to true, the timer will reset it
  @volatile
  protected var changed = false

  @volatile
  protected var ttInst: Short = 0

  protected val tt = new TimerTask {
    protected var didTimeOut = 0
    @inline final protected def canRun = active && instance == ttInst
    def run(to: Timeout) {
      if (canRun) {
        if (changed) {
          changed = false
          didTimeOut -= 1
        } else {
          lock.lock()
          if (!canRun) {
            lock.unlock()
            return
          }
          try {
            if (!changed) {
              Logger("ToPredicate", Debug, "delivering because of timeout: instance " + instance + ", round " + currentRound)
              didTimeOut += 1
              deliver
            } else {
              didTimeOut -= 1
              changed = false
            }
          } finally {
            lock.unlock()
          }
        }
        if (adaptative) {
          //TODO something amortized to avoid oscillations
          if (didTimeOut > 5) {
            defaultTO += 10
          } else if (didTimeOut < 10) {
            defaultTO -= 10
          }
        }
        timeout = Timer.newTimeout(this, defaultTO)
      }
    }
  }
  protected var timeout: Timeout = null

  override def start(g: Group, inst: Short, p: RtProcess, m: Set[Message]) {
    lock.lock()
    try {
      changed = false
      super.start(g, inst, p, m)
      ttInst = instance
      timeout = Timer.newTimeout(tt, defaultTO)
    } finally {
      lock.unlock()
    }
  }

  override def stop(inst: Short) {
    lock.lock()
    try {
      if (active && instance == inst && timeout != null) {
        timeout.cancel
      }
      super.stop(inst)
    } finally {
      lock.unlock()
    }
  }

  override protected def storePacket(pkt: DatagramPacket) {
    assert(active, "active")
    super.storePacket(pkt)
    changed = true
  }

  def receive(pkt: DatagramPacket) {
    val tag = Message.getTag(pkt.content)
    val round = tag.roundNbr
    lock.lock()
    try {
      if (instance != tag.instanceNbr) {
        pkt.release
      } else {
        while(round - currentRound > 0 && active) {
          //println(grp.self.id + ", " + tag.instanceNbr + " catching up: " + currentRound + " -> " + round)
          deliver
        }
        if (round == currentRound && active) {
          //println(grp.self.id + ", " + tag.instanceNbr + " delivering: " + currentRound)
          //normal case
          storePacket(pkt)
          if (received >= expected) {
            deliver
          }
        } else {
          //if (active) println(grp.self.id + ", " + tag.instanceNbr + " late: " + round + " -> " + currentRound)
          pkt.release //packet late or we are inactive
        }
      }
    } finally {
      lock.unlock()
    }
  }

}
