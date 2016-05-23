package example

import psync._
import psync.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.ConcurrentSkipListSet
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.TimeUnit
import scala.collection.mutable.PriorityQueue
import scala.util.Random
import io.netty.buffer.PooledByteBufAllocator

class BatchingClient(options: RuntimeOptions,
                     nbrValues: Int,
                     batchSize: Int,
                     _rate: Int,
                     eagerStart: Boolean,
                     logFile: Option[String]
                   ) extends DecisionLog[Array[Byte]]
{
  import BatchingClient.myYield

  final val Decision = 4
  final val Late = 5
  final val ForwardedBatch = 6
  final val AskDecision = 7

  val id = options.id
  val lck = new ReentrantLock 
  var nbr = 0l

  // Rate limiting (sliding window)

  var rate = _rate
  val monitor = lck.newCondition()
  /** self started instances (used for rate limiting) */
  val selfStarted = scala.collection.mutable.Set[Short]()

  val tracker = new InstanceTracking

  // logging decisions

  val log: java.io.BufferedWriter =
    if (logFile.isDefined) new java.io.BufferedWriter(new java.io.FileWriter(logFile.get + "_" + id + ".log"))
    else null

  if (log != null) {
    log.write("inst\tkey\tvalue")
    log.newLine()
  }


  // PSync runtime
  
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
    if (a != null && a.nonEmpty) { //null/empty means the proposer crashed before setting a value
      assert(a.size % 12 == 0)
      var b = 0
      while (b < a.size) {
        processRequest(inst, a, b)
        b += 12
        nbr += 1
      }
    }
  }


  // the thread that executes the commands and notify the clients
  val decisionProcessor = new Thread(new Runnable(){
    object BatchOrdering extends Ordering[(Short, Array[Byte])] {
      // assumes that the instance number are unique
      def compare(a: (Short, Array[Byte]), b: (Short, Array[Byte])) = {
        Instance.compare(b._1, a._1) // swap a,b for lowest batch first
      }
    }
    private val reorderingQueue = new PriorityQueue[(Short, Array[Byte])]()(BatchOrdering)
    private var nextBatch: Short = 1
    def run = {
      try{
        while(!Thread.interrupted) {
          val req = acceptedBatch.poll
          if (req != null) {
            val (inst, batch) = req
            if (inst == nextBatch) {
              processBatch(inst, batch)
              nextBatch = (nextBatch + 1).toShort
              // check in there are pending batches to process
              while (!reorderingQueue.isEmpty && reorderingQueue.head._1 == nextBatch) {
                val (inst, batch) = reorderingQueue.dequeue
                processBatch(inst, batch)
                nextBatch = (nextBatch + 1).toShort
              }
            } else {
              // not the next batch, put in the reordering queue
              Logger.assert(Instance.lt(nextBatch, inst), "BatchingClient", "nextBatch = " + nextBatch + ", inst = " + inst)
              reorderingQueue += req
              if (reorderingQueue.size > _rate * 10 && !tracker.isRunning(nextBatch)) {
                //pick someone to ask
                var askingTo = scala.util.Random.nextInt(rt.directory.size)
                while (askingTo == id) {
                  askingTo = scala.util.Random.nextInt(rt.directory.size)
                }
                val payload = PooledByteBufAllocator.DEFAULT.buffer()
                payload.writeLong(8)
                rt.sendMessage(new ProcessID(askingTo.toShort), Tag(nextBatch,0,AskDecision,0), payload)
                Logger("BatchingClient", Info, id + " asking to " + askingTo + " for decision " + nextBatch)
              }
            }
          } else {
            myYield
          }
        }
      } catch {
        case _: java.lang.InterruptedException => ()
      }
      Logger("BatchingClient", Info, "decisionProcessor, nextBatch = " + nextBatch + ", |reorderingQueue| = " + reorderingQueue.size)
    }
  })
  decisionProcessor.start


  @inline final def submitBatch(batch: Array[Byte]) {
    if (id == 0) {
      lck.lock
      try {
        Logger("BatchingClient", Debug, id + ", taking")
        while(rate <= 0) {
          monitor.await
        }
        val inst = (tracker.started + 1).toShort
        selfStarted.add(inst)
        rate -= 1
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
              myYield
            }
          }
        }
      } catch {
        case _: java.lang.InterruptedException => ()
      }
      Logger("BatchingClient", Info, "requestsProcessor, |pendingRequests| = " + pendingRequests.size + ", |pendingBatch| = " + pendingBatch.size)
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
    assert(tracker.canStart(inst) && !tracker.isRunning(inst))
    tracker.start(inst)
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
        acceptedBatch.add(inst -> data)
        //
        lck.lock
        try {
          tracker.stop(inst)
          if (selfStarted contains inst) {
            Logger("BatchingClient", Debug, id + ", releasing")
            selfStarted -= inst
            rate += 1
            monitor.signal()
          }
        } finally {
          lck.unlock
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
        if (tracker.canStart(inst)) {
          start(inst, emp, Set(msg))
        } else if (tracker.isRunning(inst)) {
          if (!rt.deliverMessage(msg)) {
            Logger("BatchingClient", Debug, "could not deliver message message for running instance " + inst)
            msg.release
          }
        } else {
          sendRecoveryInfo(inst, msg.senderId)
          msg.release
        }
      } finally {
        lck.unlock
      }
    } else if (flag == AskDecision) {
      val inst = msg.instance
      lck.lock
      try {
        if (tracker.canStart(inst) || tracker.isRunning(inst)) {
          if (tracker.pending(inst)) {
            Logger("BatchingClient", Info, id + ", AskDecision for pending instance " + inst)
          } else if (tracker.running(inst)) {
            Logger("BatchingClient", Info, id + ", AskDecision for running instance " + inst)
          } else if (Instance.lt(tracker.started, inst)){
            Logger("BatchingClient", Info, id + ", AskDecision for instance not yet started " + inst)
          } else {
            Logger("BatchingClient", Warning, id + ", AskDecision for instance " + inst + "\n" + tracker)
          }
        } else {
          sendRecoveryInfo(inst, msg.senderId)
        }
        msg.release
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
      msg.release
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

  def sendRecoveryInfo(inst: Short, dest: ProcessID) = {
    getDec(inst) match {
      case Some(d) =>
        val payload = PooledByteBufAllocator.DEFAULT.buffer()
        payload.writeLong(8)
        if (d.nonEmpty) {
          payload.writeInt(d.size)
          payload.writeBytes(d)
        } else {
          payload.writeInt(emp.size)
          payload.writeBytes(emp)
        }
        rt.sendMessage(dest, Tag(inst,0,Decision,0), payload)
        Logger("BatchingClient", Debug, id + " sending decision to " + dest.id + " for " + inst)
      case None =>
        val payload = PooledByteBufAllocator.DEFAULT.buffer()
        payload.writeLong(8)
        rt.sendMessage(dest, Tag(inst,0,Late,0), payload)
        //TODO send the whole state
        Logger("BatchingClient", Debug, id + " sending late to " + dest.id + " for " + inst)
    }
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

  def myYield = {
    Thread.`yield`()
    //Thread.sleep(5)
  }

  var confFile = "src/test/resources/sample-conf.xml"
  
  var logFile: Option[String] = None
  newOption("--log", dzufferey.arg.String(str => logFile = Some(str) ), "log file prefix")

  var eagerStart = false
  newOption("--eager", dzufferey.arg.Unit( () => eagerStart = true), "start the instances in parallel")

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
    system = new BatchingClient(this, n, batch, rate, eagerStart, logFile)

    //let the system setup before starting
    Thread.sleep(delay)
    begin = java.lang.System.currentTimeMillis()

    val prng = new util.Random()

    //TODO many clients
    while (true) {
      var i = 0
      while (i < 150) {
        system.propose(id, prng.nextInt(n), prng.nextInt())
        i += 1
      }
      Thread.sleep(1)
    }

  }
  
  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        val versionNbr = system.shutdown
        val end = java.lang.System.currentTimeMillis()
        val duration = (end - begin) / 1000
        Logger("BatchingClient", Notice, "#decisions = " + versionNbr + ", Δt = " + duration + ", throughput = " + (versionNbr/duration))
      }
    }
  )


}


/** To keep track of the what is running
 *  This object is not thread safe! */
class InstanceTracking {

  /** the # of the highest started instance */
  var started: Short = 0
  /** what is currently running */
  var running = scala.collection.mutable.Set[Short]()
  /** with the sliding window, there may be gap (# < started but not yet started) */
  var pending = scala.collection.mutable.Set[Short]()

  assertTrackingInvariant

  override def toString = {
    "started: " + started +
    "\nrunning: " + running.mkString(", ") +
    "\npending: " + pending.mkString(", ")
  }

  def canStart(inst: Short) = {
    Instance.lt(started, inst) || pending(inst)
  }

  def start(inst: Short) {
    var oldStarted = started
    started = Instance.max(started, inst)
    pending -= inst
    running += inst
    oldStarted = (oldStarted + 1).toShort
    while(Instance.lt(oldStarted, started)) {
      pending += oldStarted
      oldStarted = (oldStarted + 1).toShort
    }
    assertTrackingInvariant
  }

  def stop(inst: Short) {
    assert(running(inst), "not running " + inst + "\n" + toString)
    running -= inst
    assertTrackingInvariant
  }

  def isRunning(inst: Short) = running contains inst

  def trackingInvariant = {
    running.forall( Instance.leq(_, started) ) &&
    pending.forall( Instance.lt(_, started) ) &&
    pending.forall( !running.contains( _ ) )
  }

  def assertTrackingInvariant {
    if (!trackingInvariant) {
      Logger.logAndThrow("InstanceTracking", Error, toString)
    }
  }

}
