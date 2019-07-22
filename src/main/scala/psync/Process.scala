package psync

import psync.runtime.Group
import psync.utils.serialization.{KryoByteBufInput, KryoByteBufOutput}
import com.esotericsoftware.kryo.Kryo
import psync.formula.Formula


abstract class Process[IO] extends RtProcess {

  //TODO rewrite with a macro to get the initial state
  def init(io: IO)
  
  lazy val HO: Set[Process[IO]] = sys.error("used only for specification!")

  // for verification
  protected[psync] var initState: Option[Formula] = None

}

object Process {
  var fillInitState = false
}

/* The type indepent parts that are necessary for the runtime */
abstract class RtProcess {

  val rounds: Array[(RtRound,RoundSpec)]

  //use private variable to limit what the user can mess-up with

  private var _id: ProcessID = new ProcessID(-1)
  def id: ProcessID = _id

  private var rr: Time = new Time(-1)
  def r: Time = rr

  private var _r: Int = -1

  private var _n: Int = 0
  def n: Int = _n

  private var packetSize = -1

  protected[psync] def setGroup(g: Group): Unit = {
    rr = new Time(-1)
    _r = -1
    _id = g.self
    rounds.foreach(_._1.setGroup(g))
    _n = g.size
  }

  final protected def incrementRound {
    rr = rr.tick
    _r += 1
    if (_r >= rounds.length) {
      _r = 0
    }
  }

  final protected def currentRound: RtRound = rounds(_r)._1

  final protected[psync] def registerSerializer(kryo: Kryo) = {
    rounds.foreach(_._1.registerSerializer(kryo))
  }
  
  protected[psync] final def init(): Progress = {
    incrementRound
    currentRound.init
  }

  protected[psync] final def send(kryo: Kryo, alloc: Int => KryoByteBufOutput, sending: ProcessID => Unit): Progress = {
    currentRound.packSend(kryo, alloc, sending)
  }

  protected[psync] final def receive(kryo: Kryo, sender: ProcessID, payload: KryoByteBufInput): Progress = {
    currentRound.receiveMsg(kryo, sender, payload)
  }

  protected[psync] final def update(didTimeout: Boolean): Boolean = {
    currentRound.finishRound(didTimeout)
  }

}
