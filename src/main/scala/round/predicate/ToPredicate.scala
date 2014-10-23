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
      grp: Group,
      instance: Short,
      channel: Channel,
      dispatcher: InstanceDispatcher,
      proc: Process,
      options: Map[String, String] = Map.empty
    ) extends Predicate(grp, instance, channel, dispatcher, proc, options)
{

  //safety condition guaranteed by the predicate
  val ensures = round.formula.True() 

  protected var expected = n

  private val from = Array.fill(n)(false)
  private var _received = 0
  def received = _received
  def resetReceived { _received = 0 }
  //var spill = new java.util.concurrent.ConcurrentLinkedQueue[DatagramPacket]()

  private val lock = new ReentrantLock

  //dealing with the timeout ?
  protected var defaultTO = {
    try {
      options.getOrElse("timeout", "200").toInt
    } catch {
      case e: Exception =>
        Logger("Predicate", Warning, "timeout unspecified or wrong format, using 200")
        200 //milliseconds
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

  //some flag about being active
  @volatile
  protected var active = true

  //each modification should set this to true, the timer will reset it
  @volatile
  protected var changed = false

  protected var didTimeOut = 0

  protected val tt = new TimerTask {
    def run(to: Timeout) {
      if (active) {
        if (changed) {
          changed = false
          didTimeOut -= 1
        } else {
          lock.lock()
          try {
            if (!changed) {
              Logger("ToPredicate", Debug, "delivering because of timeout")
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
  protected var timeout: Timeout = Timer.newTimeout(tt, defaultTO)

  override def start {
    lock.lock()
    try {
      super.start
    } finally {
      lock.unlock()
    }
  }

  override def stop {
    active = false
    timeout.cancel
    super.stop
  }

  override protected def clear {
    super.clear
    for (i <- 0 until n) {
      from(i) = false
    }
  }

  override protected def atRoundChange {
    expected = proc.expectedNbrMessages
    Logger("ToPredicate", Debug, "expected # msg: " + expected)
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
    if (!from(id)) {
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
      //TODO take round overflow into account
      if(round == currentRound) {
        normalReceive(pkt)
      } else if (round > currentRound) {
        //we are late, need to catch up
        while(currentRound < round) {
          deliver //TODO skip the sending ?
        }
        //then back to normal
        normalReceive(pkt)
      } else {
        //late message, drop it
        pkt.release
      }
    } finally {
      lock.unlock()
    }
  }

}
