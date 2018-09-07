package psync

abstract class Process[IO] extends RtProcess {

  //TODO rewrite with a macro to get the initial state
  def init(io: IO)
  
  lazy val HO: Set[Process[IO]] = sys.error("used only for specification!")

  // for verification
  protected[psync] var initState: Option[psync.formula.Formula] = None

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

  protected[psync] def setGroup(g: psync.runtime.Group): Unit = {
    rr = new Time(-1)
    _r = -1
    _id = g.self
    rounds.foreach(_._1.setGroup(g))
    _n = g.size
  }

  protected def incrementRound {
    rr = rr.tick
    _r += 1
    if (_r >= rounds.length) {
      _r = 0
    }
  }

  protected def currentRound: RtRound = rounds(_r)._1

  protected[psync] final def send(alloc: Int => psync.utils.serialization.KryoByteBufOutput, sending: (ProcessID, psync.utils.serialization.KryoByteBufOutput) => Unit) = {
    incrementRound
    currentRound.packSend(alloc, sending)
  }

  protected[psync] final def receive(sender: ProcessID, payload: psync.utils.serialization.KryoByteBufInput): Boolean = {
    currentRound.receiveMsg(sender, payload)
  }

  protected[psync] final def update: Boolean = {
    currentRound.finishRound
  }

}
