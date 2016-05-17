package example

import psync._
import psync.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.util.concurrent.Semaphore
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.TimeUnit
import scala.util.Random
import io.netty.buffer.PooledByteBufAllocator


class BatchingClient(options: RuntimeOptions,
                     nbrValues: Int,
                     batchSize: Int,
                     _rate: Int,
                     logFile: Option[String]
                   ) extends DecisionLog[Array[Byte]]
{

  final val Decision = 4
  final val Late = 5
  final val ForwardedBatch = 6

  val id = options.id
  val lck = new ReentrantLock 
  var nbr = 0l

  //
  val rate = new Semaphore(_rate)
  //
  var selfStarted = scala.collection.mutable.Set[Short]()
  var started: Short = 0
  var finished: Short = 0


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

  // state of the system
  val values   = Array.fill[Int](nbrValues)(0)
  val versions = Array.fill[Short](nbrValues)(-1)
  // batches that have been accepted but still needs to be executed
  val acceptedBatch = new ConcurrentLinkedQueue[(Short, Array[Byte])]()
  // batches that needs to be proposed
  val pendingBatch = new ConcurrentLinkedQueue[Array[Byte]]
  // requests that still needs to be batched
  val pendingRequests = new ArrayBlockingQueue[(Int,Int,Int)](10)


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

  // the thread that executes the commands and notify the clients
  val decisionProcessor = new Thread(new Runnable(){
    def run = {
      try{
        while(!Thread.interrupted) {
          val req = acceptedBatch.poll
          if (req != null) {
            val (inst, batch) = req
            //TODO reordering of batches
            processBatch(inst, batch)
          } else {
            Thread.`yield`()
            //Thread.sleep(10)
          }
        }
      } catch {
        case _: java.lang.InterruptedException => ()
      }
    }
  })
  decisionProcessor.start


  @inline final def submitBatch(batch: Array[Byte]) {
    if (id == 0) {
      Logger("BatchingClient", Debug, id + ", taking")
      rate.acquire
      lck.lock
      try {
        val inst = (started + 1).toShort
        selfStarted += inst
        start(inst, batch, Set.empty)
      } finally {
        lck.unlock
      }
    } else {
      // forward the batch to the leader (process with id 0)
      val payload = PooledByteBufAllocator.DEFAULT.buffer()
      payload.writeLong(8)
      payload.writeInt(batch.size)
      payload.writeBytes(batch)
      rt.sendMessage(new ProcessID(0), Tag(0,0,ForwardedBatch,0), payload)
    }
  }

  // the thread that batches the requests and submit the batches
  val requestsProcessor = new Thread(new Runnable(){

    //an array to accumulate requests into batches
    val rs = batchSize * 12
    var request = Array.ofDim[Byte](rs)
    var idx = 0

    def run = {
      try{
        while(!Thread.interrupted) {
          val req = pendingRequests.poll
          if (req != null) {
            val (c,k,v) = req
            assert(idx < rs + 12)
            intToBytes(c, request, idx)
            intToBytes(k, request, idx + 4)
            intToBytes(v, request, idx + 8)
            idx += 12
            if (idx >= rs) {
              submitBatch(request)
              request = Array.ofDim[Byte](rs)
              idx = 0
            }
          } else {
            val b = pendingBatch.poll
            if (b != null) {
              submitBatch(b)
            } else {
              Thread.`yield`()
              //Thread.sleep(10)
            }
          }
        }
      } catch {
        case _: java.lang.InterruptedException => ()
      }
    }
  })
  requestsProcessor.start

  val emp = Array[Byte]()

  def start(inst: Short, data: Array[Byte], msgs: Set[Message]) = {
    assert(lck.isHeldByCurrentThread())
    val io = new BConsensusIO {
      val i = inst
      val phase: Int = 0 //inst
      val initialValue = data
      def decide(value: Array[Byte]) {
        proposeDecision(i, value)
      }
    }
    started = inst
    assert(Instance.leq(finished,started))
    rt.startInstance(inst, io, msgs)
  }

  def proposeDecision(inst: Short, data: Array[Byte]) = {
    Logger("BatchingClient", Debug, id + ", proposeDecision for " + inst)
    val l = getLock(inst)
    l.lock
    try {
      //put the decision in the rolling log
      if (pushDecision(inst, data)) {
        Logger("BatchingClient", Debug, id + ", pushDecision ✓")
        finished = inst
        assert(Instance.leq(finished,started))
        //if (selfStarted contains inst) {
        if (id == 0) {
          Logger("BatchingClient", Debug, id + ", releasing")
          selfStarted -= inst
          rate.release
        }
        if (data != null && data.nonEmpty) { //null/empty means the proposer crashed before setting a value
          acceptedBatch.add(inst -> data)
        }
      } else {
        Logger("BatchingClient", Debug, id + ", pushDecision ✗")
      }
    } finally {
      l.unlock
    }
  }
  
  def defaultHandler(msg: Message) {
    val flag = msg.tag.flag
    Logger("BatchingClient", Debug, id + ", defaultHandler: " + msg.tag)
    if (flag == Flags.normal || flag == Flags.dummy) {
      val inst = msg.instance
      lck.lock
      try {
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
              Logger("BatchingClient", Debug, "sending decision to " + msg.senderId.id + " for " + inst)
            case None =>
              val payload = PooledByteBufAllocator.DEFAULT.buffer()
              payload.writeLong(8)
              rt.sendMessage(msg.senderId, Tag(inst,0,Late,0), payload)
              Logger("BatchingClient", Debug, "sending late to " + msg.senderId.id + " for " + inst)
          }
          msg.release
        } else {
          //TODO check if running and push to inst ?
          Logger("BatchingClient", Debug, "message for instance started but not finished: " + inst + ", started: " + started + ", finished: " + finished)
          msg.release
        }
      } finally {
        lck.unlock
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
      Logger("BatchingClient", Debug, "received decision for " + inst)
    } else if (flag == Late) {
      val inst = msg.instance
      rt.stopInstance(inst)
      //TODO get the whole state
      proposeDecision(inst, null)
      Logger("BatchingClient", Debug, "received late for " + inst)
    } else if (flag == ForwardedBatch) {
      val payload = msg.payload
      payload.readLong //skip tag
      val size = payload.readInt()
      val batch = new Array[Byte](size)
      payload.readBytes(batch)
      msg.release
      pendingBatch.add(batch)
    } else {
      sys.error("unknown or error flag: " + flag)
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
        //TODO send the whole state
    }
    rt.sendMessage(sender, tag, payload)
  }
  
  def propose(c: Int, k: Int, v: Int) {
    pendingRequests.put((c,k,v))
  }

  def shutdown: Long = {
    rt.shutdown
    decisionProcessor.interrupt
    requestsProcessor.interrupt
    if (log != null) {
      log.close
    }
    nbr
  }

}


object BatchingClient extends RTOptions {

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

  var system: BatchingClient = null 

  def main(args: Array[java.lang.String]) {
    apply(args)
    system = new BatchingClient(this, n, batch, rate, logFile)

    //let the system setup before starting
    Thread.sleep(delay)
    begin = java.lang.System.currentTimeMillis()

    val prng = new util.Random()

    //TODO many clients
    while (true) {
      system.propose(id, prng.nextInt(n), prng.nextInt())
      //Thread.`yield`()
      //Thread.sleep(10)
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
