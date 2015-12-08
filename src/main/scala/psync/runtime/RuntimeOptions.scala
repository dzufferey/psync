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
  val UDP, TCP = Value
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
  def adaptative = _adaptative
  def packetSize = _packetSize
  def bufferSize = _bufferSize
  def processPool = _processPool
  def workers = _workers
  def port = _port
  def dispatch = _dispatch

  protected var _peers = List[Replica]()
  protected var _id: Short = -1
  protected var _group = NetworkGroup.NIO
  protected var _protocol = NetworkProtocol.UDP
  protected var _earlyMoving = true
  protected var _timeout = 10l
  protected var _adaptative = false
  protected var _packetSize = -1
  protected var _bufferSize = 64
  protected var _processPool = 64
  protected var _workers: Workers = Adapt
  protected var _port = -1
  protected var _dispatch = 7

}

abstract class RTOptions extends DefaultOptions with RuntimeOptions {

  import dzufferey.arg._
  
  newOption("--conf",           String( s => processConFile(s)),            "configuration file")
  newOption("-id",              Int( i => _id = i.toShort),                 "the replica ID")
  newOption("--id",             Int( i => _id = i.toShort),                 "the replica ID")
  newOption("--group",          String( s => _group = parseGroup(s)),       "the network layer interface used by Netty: nio/oio/epoll (default: nio).")
  newOption("--protocol",       String( s => _protocol = parseProtocol(s)), "the network protocol: udp/tcp (default: udp).")
  newOption("-to",              Int( i => _timeout = i.toLong ),            "default timeout for runtime (default: 10).")
  newOption("--timeout",        Int( i => _timeout = i.toLong ),            "default timeout for runtime (default: 10).")
  newOption("--earlyMoving",    Bool( b => _earlyMoving = b ),              "early moving optimization (default: true).")
  newOption("--noEarlyMoving",  Unit( () => _earlyMoving = false ),         "disable early moving optimization.")
  newOption("--adapt",          Unit( () => _adaptative = true ),           "adaptative timeout (default: false).")
  newOption("--adaptative",     Bool( b => _adaptative = b ),               "adaptative timeout (default: false).")
  newOption("--packetSize",     Int( i => _packetSize = i ),                "max packet size for the memory allocator (default: what netty provides).")
  newOption("--bufferSize",     Int( i => _bufferSize = i ),                "size of the buffer for each instancer (default: 64).")
  newOption("--processPool",    Int( i => _processPool = i ),               "how many processes are allocated at start (default: 64).")
  newOption("--workers",        String( s => _workers = parseWorkers(s) ),  "number of workers: adaptative/\\d(fixed)/\\dx(coeff on #CPU) (default: adaptative).")
  newOption("--port",           Int( i => _port = i ),                      "port number, in case we don't know which replica we are.")
  newOption("--dispatch",       Int( i => _dispatch = i ),                  "log₂ fan out of the dispatcher (default: 7).")

  def processConFile(s: java.lang.String) {
    val (ps, opts) = Config.parse(s)
    _peers = ps
    var args = List[java.lang.String]()
    for ( (key, value) <- opts ) {
      args = value :: ("--" + key) :: args
    }
    apply(args.reverse) //preserve the ordering
  }

  def parseGroup(s: java.lang.String) = s.toLowerCase match {
    case "nio" => NetworkGroup.NIO
    case "oio" => NetworkGroup.OIO
    case "epoll" => NetworkGroup.EPOLL
    case other =>
      Logger("RuntimeOptions", Warning, "NetworkGroup, unknown '"+other+"' using NIO")
      NetworkGroup.NIO
  }
  
  def parseProtocol(s: java.lang.String) = s.toLowerCase match {
    case "udp" => NetworkProtocol.UDP
    case "tcp" => NetworkProtocol.TCP
    case other =>
      Logger("RuntimeOptions", Warning, "NetworkProtocol, unknown '"+other+"' using UDP")
      NetworkProtocol.UDP
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
