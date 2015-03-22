package example

import round._
import round.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.util.concurrent.Semaphore
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.ConcurrentSkipListSet
import scala.util.Random
import io.netty.buffer.PooledByteBufAllocator

class PerfTest3(id: Int,
                confFile: String,
                _rate: Short,
                logFile: Option[String],
                additionalOptions: Map[String,String]
               ) extends DecisionLog[Array[Byte]]
{

  final val Decision = 4
  final val Late = 5

  val rate = new Semaphore(_rate)

  val log: java.io.BufferedWriter =
    if (logFile.isDefined) new java.io.BufferedWriter(new java.io.FileWriter(logFile.get + "_" + id + ".log"))
    else null
  val logLock = new ReentrantLock

  if (log != null) {
    log.write("idx\tinst\tval")
    log.newLine()
  }
  
  val alg = new LastVoting2B
  val rt = new RunTime(alg)
  rt.startService(defaultHandler(_), confFile, additionalOptions + ("id" -> id.toString))

  val lck = new ReentrantLock 
  //val nbr = new AtomicLong(0l)
  var nbr = 0l
  var started: Short = 0

  val emp = Array[Byte]()

  def start(inst: Short, data: Array[Byte], msgs: Set[Message]) = {
    val io = new BConsensusIO {
      val i = inst
      val initialValue = emp
      def decide(value: Array[Byte]) {
        proposeDecision(i, value)
      }
    }
    nbr = nbr + 1
    started = inst
    rt.startInstance(inst, io, msgs)
  }

  def proposeDecision(inst: Short, data: Array[Byte]) = {
    val l = getLock(inst)
    l.lock
    try {
      if (pushDecision(inst, data)) {
        if (id == 0) {
          rate.release
        }
        if (log != null && data != null) {
          //TODO log
        }
      }
    } finally {
      l.unlock
    }
  }
  
  def defaultHandler(msg: Message) {
    lck.lock
    try {
      val flag = msg.tag.flag
      if (flag == Flags.normal || flag == Flags.dummy) {
        val inst = msg.instance
        if (Instance.lt(started, inst)) {
          start(inst, emp, Set(msg))
        } else {
          //TODO check if running and push to inst ?
          msg.release
        }
      } else if (flag == Decision) {
        val inst = msg.instance
        val p = msg.payload
        p.readLong //skip tag
        val s = p.readInt
        val d = Array.ofDim[Byte](s)
        p.readBytes(d)
        msg.release
        rt.stopInstance(inst)
        proposeDecision(inst, d)
      } else if (flag == Late) {
        val inst = msg.instance
        rt.stopInstance(inst)
        proposeDecision(inst, null)
      } else {
        sys.error("unknown or error flag: " + flag)
      }
    } finally {
      lck.unlock
    }
  }
  
  def sendRecoveryInfo(m: Message) = {
    val inst = m.instance
    val payload = PooledByteBufAllocator.DEFAULT.buffer()
    val sender = m.senderId
    payload.writeLong(8)
    var tag = Tag(0,0)
    getDec(inst) match {
      case Some(d) =>
        tag = Tag(inst,0,Decision,0)
        payload.writeInt(d.size)
        payload.writeBytes(d)
      case None =>
        Tag(inst,0,Late,0)
    }
    rt.sendMessage(sender, tag, payload)
  }
  
  def wakeupOthers(inst: Short) {
    //TODO better way
    val dir = rt.directory
    for (o <- dir.others) {
      val payload = PooledByteBufAllocator.DEFAULT.buffer()
      payload.writeLong(8)
      var tag = Tag(inst,0,Flags.dummy,0)
      rt.sendMessage(o.id, tag, payload)
    }
  }

  val prng = new util.Random(1111)

  def propose(batchSize: Int, reqSize: Int) = {
    val request = Array.ofDim[Byte](batchSize * reqSize)
    prng.nextBytes(request)
    rate.acquire
    lck.lock
    try {
      val i = (started + 1).toShort
      start(i, request, Set.empty)
      wakeupOthers(i)
    } finally {
      lck.unlock
    }
  }
  
  def shutdown: Long = {
    rt.shutdown
    if (log != null) {
      log.close
    }
    nbr
  }

}

object PerfTest3 extends round.utils.DefaultOptions {

  var id = -1
  newOption("-id", dzufferey.arg.Int( i => id = i), "the replica ID")

  var confFile = "src/test/resources/sample-conf.xml"
  newOption("--conf", dzufferey.arg.String(str => confFile = str ), "config file")
  
  var logFile: Option[String] = None
  newOption("--log", dzufferey.arg.String(str => logFile = Some(str) ), "log file prefix")

  var rate = 5
  newOption("-rt", dzufferey.arg.Int( i => rate = i), "fix the rate (#queries in parallel)")

  var to = -1
  newOption("-to", dzufferey.arg.Int( i => to = i), "timeout (default in config file)")
  
  var delay = 1000
  newOption("-delay", dzufferey.arg.Int( i => delay = i), "delay in ms before making queries (allow the replicas to setup)")

  var batch = 10
  newOption("-b", dzufferey.arg.Int( i => batch = i), "batch size")

  var req = 16
  newOption("-r", dzufferey.arg.Int( i => req = i), "request size")

  val usage = "..."
  
  var begin = 0l

  var system: PerfTest3 = null 

  def main(args: Array[java.lang.String]) {
    apply(args)
    val opts =
      if (to > 0) Map("timeout" -> to.toString)
      else Map[String, String]()
    system = new PerfTest3(id, confFile, rate.toShort, logFile, opts)

    //let the system setup before starting
    Thread.sleep(delay)
    begin = java.lang.System.currentTimeMillis()

    //makes queries ...
    while (true) {
      if (id == 0) {
        system.propose(batch, req)
      } else {
        Thread.sleep(1000)
      }
    }

  }
  
  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        val versionNbr = system.shutdown
        val end = java.lang.System.currentTimeMillis()
        val duration = (end - begin) / 1000
        println("#instances = " + versionNbr + ", Δt = " + duration + ", throughput = " + (versionNbr/duration))
      }
    }
  )


}
