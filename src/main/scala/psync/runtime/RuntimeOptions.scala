package psync.runtime

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import psync.utils.DefaultOptions

object NetworkGroup extends Enumeration {
  type NetworkGroup = Value
  val NIO, OIO, EPOLL = Value
}

object NetworkProtocol extends Enumeration {
  type NetworkProtocol = Value
  val UDP, TCP, TCP_SSL = Value
}

sealed abstract class Workers
case object Adapt extends Workers
case class Fixed(nbr: Int) extends Workers
case class Factor(coeff: Int) extends Workers

trait RuntimeOptions {

  def id = _id
  def peers = _peers
  def group = _group
  def protocol = _protocol
  def timeout = _timeout
  def earlyMoving = _earlyMoving
  def sendWhenCatchingUp = _sendWhenCatchingUp
  def delayFirstSend = _delayFirstSend
  def adaptative = _adaptative
  def packetSize = _packetSize
  def bufferSize = _bufferSize
  def processPool = _processPool
  def workers = _workers
  def port = _port
  def dispatch = _dispatch
  def connectionRestartPeriod = _connectionRestartPeriod
  def acceptUnknownConnection = _acceptUnknownConnection

  protected var _peers = List[Replica]()
  protected var _id: Short = -1
  protected var _group = NetworkGroup.NIO
  protected var _protocol = NetworkProtocol.UDP
  protected var _earlyMoving = true
  protected var _sendWhenCatchingUp = true
  protected var _delayFirstSend = -1
  protected var _timeout = 10l
  protected var _adaptative = false
  protected var _packetSize = -1
  protected var _bufferSize = 64
  protected var _processPool = 32
  protected var _workers: Workers = Adapt
  protected var _port = -1
  protected var _dispatch = 6
  protected var _connectionRestartPeriod = 250
  protected var _acceptUnknownConnection = false

}

abstract class RTOptions extends DefaultOptions with RuntimeOptions {

  import dzufferey.arg._

  newOption("--conf",                   String( s => processConFile(s)),            "configuration file")
  newOption("-id",                      Int( i => _id = i.toShort),                 "the replica ID")
  newOption("--id",                     Int( i => _id = i.toShort),                 "the replica ID")
  newOption("--group",                  Enum(NetworkGroup, (s: NetworkGroup.NetworkGroup) => _group = s),
                                                                                    "the network layer interface used by Netty: NIO/OIO/EPOLL (default: NIO).")
  newOption("--protocol",               Enum(NetworkProtocol, (s: NetworkProtocol.NetworkProtocol) => _protocol = s),
                                                                                    "the network protocol: UDP/TCP/TCP_SSL (default: UDP).")
  newOption("-to",                      Int( i => _timeout = i.toLong ),            "default timeout for runtime (default: 10).")
  newOption("--timeout",                Int( i => _timeout = i.toLong ),            "default timeout for runtime (default: 10).")
  newOption("--earlyMoving",            Bool( b => _earlyMoving = b ),              "early moving optimization (default: true).")
  newOption("--noEarlyMoving",          Unit( () => _earlyMoving = false ),         "disable early moving optimization.")
  newOption("--noSendWhenCatchingUp",   Unit( () => _sendWhenCatchingUp = false ),  "disable send when catching up.")
  newOption("--delayFirstSend",         Int( i => _delayFirstSend = i ),            "delay the messages send in the first round (default: -1).")
  newOption("--adapt",                  Unit( () => _adaptative = true ),           "adaptative timeout (default: false).")
  newOption("--adaptative",             Bool( b => _adaptative = b ),               "adaptative timeout (default: false).")
  newOption("--packetSize",             Int( i => _packetSize = i ),                "max packet size for the memory allocator (default: what netty provides).")
  newOption("--bufferSize",             Int( i => _bufferSize = i ),                "size of the buffer for each instancer (default: 64).")
  newOption("--processPool",            Int( i => _processPool = i ),               "how many processes are allocated at start (default: 32).")
  newOption("--workers",                String( s => _workers = parseWorkers(s) ),  "number of workers: adaptative/\\d(fixed)/\\dx(coeff on #CPU) (default: adaptative).")
  newOption("--port",                   Int( i => _port = i ),                      "port number, in case we don't know which replica we are.")
  newOption("--dispatch",               Int( i => _dispatch = i ),                  "log₂ fan out of the dispatcher (default: 6).")
  newOption("--connectionRestartPeriod",Int( i => _connectionRestartPeriod= i ),    "waiting time before trying to reconnecting for TCP (default: 250).")
  newOption("--acceptUnknownConnection",Bool( b => _acceptUnknownConnection = b ),  "accpect TCP connection from unkown replicas (default: false).")

  def processConFile(s: java.lang.String) {
    val (ps, opts) = Config.parse(s)
    _peers = ps
    var args = List[java.lang.String]()
    for ( (key, value) <- opts ) {
      args = value :: ("--" + key) :: args
    }
    apply(args.reverse) //preserve the ordering
  }

  def parseWorkers(s: java.lang.String) = {
    val n = s.toLowerCase
    try {
      if (n == "adaptative")    Adapt
      else if (n endsWith "x")  Factor(n.substring(0, n.length -1).toInt)
      else                      Fixed(n.toInt)
    } catch {
      case e: Exception =>
        Logger("RuntimeOptions", Warning, "size of pool of workers has wrong format, using adaptative")
        Adapt
    }
  }

}
