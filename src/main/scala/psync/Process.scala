package psync

abstract class Process[IO] extends RtProcess {

  //TODO rewrite with a macro to get the initial state
  def init(io: IO)
  
  lazy val HO: Set[Process[IO]] = sys.error("used only for specification!")

  // for the runtime
  def setOptions(options: runtime.RuntimeOptions) {
    rounds.foreach(_.setOptions(options))
  }

  //////////////////////
  // for verification //
  //////////////////////

  var initState: Option[psync.formula.Formula] = None

}

object Process {
  var fillInitState = false
}

/* The type indepent parts that are necessary for the runtime */
abstract class RtProcess {

  val rounds: Array[RtRound]

  //use private variable to limit what the user can mess-up with

  private var _id: ProcessID = new ProcessID(-1)
  def id: ProcessID = _id

  private var rr: Time = new Time(-1)
  def r: Time = rr

  private var _r: Int = -1

  private var _n: Int = 0
  def n: Int = _n

  protected[psync] def setGroup(g: psync.runtime.Group): Unit = {
    rr = new Time(-1)
    _r = -1
    _id = g.self
    rounds.foreach(_.setGroup(g))
    _n = g.size
  }

  protected def setOptions(options: runtime.RuntimeOptions): Unit
  
  protected def incrementRound {
    rr = rr.tick
    _r += 1
    if (_r >= rounds.length) {
      _r = 0
    }
  }

  protected def currentRound: RtRound = rounds(_r)

  protected var allocator: io.netty.buffer.ByteBufAllocator = null
  protected[psync] def setAllocator(a: io.netty.buffer.ByteBufAllocator) {
    allocator = a
  }

  final def send(): Map[ProcessID, io.netty.buffer.ByteBuf] = {
    incrementRound
    currentRound.packSend(allocator)
  }

  final def update(msgs: Map[ProcessID, io.netty.buffer.ByteBuf]): Boolean = {
    currentRound.unpackUpdate(msgs)
  }

  final def expectedNbrMessages: Int = currentRound.expectedNbrMessages

}
