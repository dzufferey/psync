package example

import psync._
import psync.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.util.concurrent.Semaphore
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.ConcurrentSkipListSet
import java.util.concurrent.TimeUnit
import scala.util.Random
import io.netty.buffer.PooledByteBufAllocator

class PerfTest3(options: RuntimeOptions,
                nbrValues: Int,
                batchSize: Int,
                _rate: Short,
                logFile: Option[String]
               ) extends DecisionLog[Array[Byte]]
{

  final val Decision = 4
  final val Late = 5

  val id = options.id
  val rate = new Semaphore(_rate)
  var selfStarted = scala.collection.mutable.Set[Short]()

  val log: java.io.BufferedWriter =
    if (logFile.isDefined) new java.io.BufferedWriter(new java.io.FileWriter(logFile.get + "_" + id + ".log"))
    else null

  if (log != null) {
    log.write("inst\tkey\tvalue")
    log.newLine()
  }
  
  val alg = new LastVotingB
  val rt = new Runtime(alg, options, defaultHandler(_))
  rt.startService

  val lck = new ReentrantLock 
  var nbr = 0l
  var started: Short = 0
  var finished: Short = 0

  val values   = Array.fill[Int](nbrValues)(0)
  val versions = Array.fill[Short](nbrValues)(-1)
  val acceptedRequests = new ConcurrentLinkedQueue[(Short, Array[Byte])]()

  @inline final def bytesToInt(b: Array[Byte], base: Int) = {
     b(base + 3) & 0xFF |
    (b(base + 2) & 0xFF) << 8 |
    (b(base + 1) & 0xFF) << 16 |
    (b(base    ) & 0xFF) << 24;
  }

  @inline final def intToBytes(a: Int, b: Array[Byte], base: Int) = {
    b(base + 3) = (a & 0xFF).toByte
    b(base + 2) = ((a >>  8) & 0xFF).toByte
    b(base + 1) = ((a >> 16) & 0xFF).toByte
    b(base    ) = ((a >> 24) & 0xFF).toByte
  }

  @inline final def processRequest(inst: Short, b: Array[Byte], base: Int) {
    val c = bytesToInt(b, base) 
    val k = bytesToInt(b, base + 4) 
    val v = bytesToInt(b, base + 8)
    //check that the request has not been superseded
    if (Instance.lt(versions(k), inst)) {
      values(k) = v
      versions(k) = inst
      if (log != null) {
        log.write(inst + "\t" + k + "\t" + v)
        log.newLine()
      }
    }
    //TODO notify client
  }

  def processBatch(inst: Short, a: Array[Byte]) {
    assert(a.size % 12 == 0)
    var b = 0
    while (b < a.size) {
      processRequest(inst, a, b)
      b += 12
      nbr += 1
    }
  }

  val reqProcessor = new Thread(new Runnable(){
    def run = {
      try{
        while(!Thread.interrupted) {
          val req = acceptedRequests.poll
          if (req != null) {
            val (inst, batch) = req
            processBatch(inst, batch)
          } else {
            Thread.sleep(2)
          }
        }
      } catch {
        case _: java.lang.InterruptedException => ()
      }
    }
  })
  reqProcessor.start

  val emp = Array[Byte]()

  def start(inst: Short, data: Array[Byte], msgs: Set[Message]) = {
    val io = new BConsensusIO {
      val phase: Int = inst
      val initialValue = data
      def decide(value: Array[Byte]) {
        proposeDecision(phase.toShort, value)
      }
    }
    started = inst
    assert(Instance.leq(finished,started))
    rt.startInstance(inst, io, msgs)
  }

  def proposeDecision(inst: Short, data: Array[Byte]) = {
    val l = getLock(inst)
    l.lock
    try {
      if (pushDecision(inst, data)) {
        finished = inst
        assert(Instance.leq(finished,started))
        if (selfStarted contains inst) {
          selfStarted -= inst
          rate.release
        }
        if (data != null && data.nonEmpty) { //null/empty means the proposer crashed before setting a value
          acceptedRequests.add(inst -> data)
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
        } else if (Instance.leq(inst, finished)) {
          getDec(inst) match {
            case Some(d) => 
              val payload = PooledByteBufAllocator.DEFAULT.buffer()
              payload.writeLong(8)
              payload.writeInt(d.size)
              payload.writeBytes(d)
              rt.sendMessage(msg.senderId, Tag(inst,0,Decision,0), payload)
              Logger("PerfTest3", Debug, "sending decision to " + msg.senderId.id + " for " + inst)
            case None =>
              val payload = PooledByteBufAllocator.DEFAULT.buffer()
              payload.writeLong(8)
              rt.sendMessage(msg.senderId, Tag(inst,0,Late,0), payload)
              Logger("PerfTest3", Debug, "sending late to " + msg.senderId.id + " for " + inst)
          }
          msg.release
        } else {
          //TODO check if running and push to inst ?
          Logger("PerfTest3", Debug, "message for instance started but not finished: " + inst + ", started: " + started + ", finished: " + finished)
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
        Logger("PerfTest3", Debug, "received decision for " + inst)
      } else if (flag == Late) {
        val inst = msg.instance
        rt.stopInstance(inst)
        proposeDecision(inst, null)
        Logger("PerfTest3", Debug, "received late for " + inst)
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
    val dir = rt.getGroup
    for (o <- dir.others) {
      val payload = PooledByteBufAllocator.DEFAULT.buffer()
      payload.writeLong(8)
      var tag = Tag(inst,0,Flags.dummy,0)
      rt.sendMessage(o.id, tag, payload)
    }
  }

  val rs = batchSize * 12
  var request = Array.ofDim[Byte](rs)
  var idx = 0

  def propose(c: Int, k: Int, v: Int) {
    if (idx < rs) {
      intToBytes(c, request, idx)
      intToBytes(k, request, idx + 4)
      intToBytes(v, request, idx + 8)
      idx += 12
    }
    if (idx >= rs) {
      rate.acquire
      lck.lock
      try {
        val inst = (started + 1).toShort
        selfStarted += inst
        start(inst, request, Set.empty)
        wakeupOthers(inst)
        idx = 0
        request = Array.ofDim[Byte](rs)
      } finally {
        lck.unlock
      }
    }
  }

  def shutdown: Long = {
    rate.drainPermits() // try to reduce the error messages when shutting down
    rt.shutdown
    reqProcessor.interrupt
    if (log != null) {
      log.close
    }
    nbr
  }

}

object PerfTest3 extends RTOptions {

  var confFile = "src/test/resources/sample-conf.xml"
  
  var logFile: Option[String] = None
  newOption("--log", dzufferey.arg.String(str => logFile = Some(str) ), "log file prefix")

  var rate = 1
  newOption("-rt", dzufferey.arg.Int( i => rate = i), "fix the rate (#queries in parallel)")

  var n = 50
  newOption("-n", dzufferey.arg.Int( i => n = i), "number of different values that we can modify")

  var delay = 1000
  newOption("-delay", dzufferey.arg.Int( i => delay = i), "delay in ms before making queries (allow the replicas to setup)")

  var batch = 100
  newOption("-b", dzufferey.arg.Int( i => batch = i), "batch size")

  val usage = "..."
  
  var begin = 0l

  var system: PerfTest3 = null 

  def main(args: Array[java.lang.String]) {
    apply(args)
    system = new PerfTest3(this, n, batch, rate.toShort, logFile)

    //let the system setup before starting
    Thread.sleep(delay)
    begin = java.lang.System.currentTimeMillis()

    val prng = new util.Random()

    Thread.sleep(prng.nextInt(10))
    //TODO more than one proposer
    while (true) {
      if(id == 0) {
        system.propose(id, prng.nextInt(n), prng.nextInt())
      } else {
        Thread.sleep(100)
      }
    }

  }
  
  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        val versionNbr = system.shutdown
        val end = java.lang.System.currentTimeMillis()
        val duration = (end - begin) / 1000
        println("#decisions = " + versionNbr + ", Δt = " + duration + ", throughput = " + (versionNbr/duration))
      }
    }
  )


}
