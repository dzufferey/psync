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

  //safety condition guaranteed by the predicate
  val ensures = round.formula.True() 

  protected var expected = 0

  private var from: Array[Boolean] = null
  private var _received = 0
  def received = _received
  def resetReceived { _received = 0 }

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
              //Logger("ToPredicate", Debug, "delivering because of timeout: " + instance)
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
      expected = n
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

  override protected def clear {
    super.clear
    for (i <- 0 until n) {
      from(i) = false
    }
  }

  override protected def atRoundChange {
    expected = proc.expectedNbrMessages
    //Logger("ToPredicate", Debug, "expected # msg: " + expected)
  }

  override protected def afterSend {
    if (received >= expected) {
      deliver
    }
  }

  
  protected def normalReceive(pkt: DatagramPacket) {
    assert(lock.isHeldByCurrentThread, "lock.isHeldByCurrentThread")
    val id = grp.inetToId(pkt.sender).id
    //protect from duplicate packet
    if (!from(id) && active) {
      from(id) = true
      messages(received) = pkt
      _received += 1
      if (received >= expected) {
        deliver
      }
      changed = true
    } else {
      pkt.release
    }
  }

  def receive(pkt: DatagramPacket) {
    val tag = Message.getTag(pkt.content)
    val round = tag.roundNbr
    lock.lock()
    try {
      if (!active || instance != tag.instanceNbr) {
        pkt.release
      } else if (round == currentRound) { //TODO take round overflow into account
        normalReceive(pkt)
      } else if (round > currentRound) {
        //we are late, need to catch up
        while(currentRound < round && active) {
          deliver //TODO skip the sending ?
        }
        //deliver might stop the instance
        if (active) {
          //then back to normal
          normalReceive(pkt)
        } else {
          pkt.release
        }
      } else {
        //late message, drop it
        pkt.release
      }
    } finally {
      lock.unlock()
    }
  }

}
