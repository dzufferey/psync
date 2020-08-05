package example.byzantine.test

//TODO borrowed from Runner

import psync._
import psync.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.{AtomicInteger,AtomicBoolean}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.ConcurrentSkipListSet
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.TimeUnit
import scala.collection.mutable.PriorityQueue
import scala.util.Random
import io.netty.buffer.PooledByteBufAllocator
import example.{DecisionLog,ConsensusIO}
import example.batching.InstanceTracking

class Runner(val options: Runner.type)
    extends DecisionLog[Array[Byte]]
    with DecisionProcessor
    with RequestProcessor
    with RateLimiting
    with InstanceTracking
{
  import Runner.{AskDecision,Decision,Late,ForwardedBatch}

  val id = options.id
  def isLeader = id == 0
  /** record the number of decision */
  var nbr = 0L

  // concurrency control
  val lck = new ReentrantLock 
  val monitor = lck.newCondition()
  val isLate = new AtomicBoolean(false)

  // PSync runtime
  val rt = Runtime(options, defaultHandler(_))
  val alg = new ConsensusAlgo(rt, options.sync, options.timeout, options.shortTO)
  var jitting = true


  final val emp = Array[Byte]()

  def startInstance(inst: Short, data: Array[Byte], msgs: Set[Message]): Unit = {
    assert(lck.isHeldByCurrentThread())
    val io = new ConsensusIO[Array[Byte]] {
      val i = inst
      val initialValue = data
      def decide(value: Array[Byte]): Unit = {
        proposeDecision(i, value)
      }
    }
    assert(canStart(inst) && !isRunning(inst))
    start(inst)
    alg.startInstance(inst, io, msgs)
  }
  
  def defaultHandler(msg: Message): Unit = {


    val flag = msg.tag.flag
    Logger("Runner", Debug, s"$id, defaultHandler: ${msg.tag}")
    if (flag == Flags.normal || flag == Flags.dummy) {
      val inst = msg.instance
      lck.lock
      try {
        if (jitting) {
          msg.release
        } else if (canStart(inst)) {
          // with eagerStart, instances are started eagerly, not lazily
          if (isLate.get) {
            //late: focus on recovery rather than starting instances
            msg.release
          } else if (!options.eagerStart) {
            startInstance(inst, emp, Set(msg))
          } else {
            msg.release
          }
        } else if (isRunning(inst)) {
          if (!rt.deliverMessage(msg)) {
            Logger("Runner", Debug, "could not deliver message message for running instance " + inst)
            msg.release
          }
        } else {
          sendRecoveryInfo(inst, msg.sender)
          msg.release
        }
      } finally {
        lck.unlock
      }
    } else if (flag == AskDecision) {
      val inst = msg.instance
      lck.lock
      try {
        if (canStart(inst) || isRunning(inst)) {
          if (pending(inst)) {
            Logger("Runner", Info, s"$id, AskDecision for pending instance $inst")
          } else if (running(inst)) {
            Logger("Runner", Info, s"$id, AskDecision for running instance $inst")
          } else if (Instance.lt(started, inst)){
            Logger("Runner", Info, s"$id, AskDecision for instance not yet started $inst")
          } else {
            Logger("Runner", Warning, s"$id, AskDecision for instance $inst")
          }
        } else {
          sendRecoveryInfo(inst, msg.sender)
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
      alg.stopInstance(inst)
      proposeDecision(inst, d)
      Logger("Runner", Debug, "received decision for " + inst)
    } else if (flag == Late) {
      val inst = msg.instance
      msg.release
      alg.stopInstance(inst)
      //TODO get the whole state
      proposeDecision(inst, null)
      Logger("Runner", Debug, "received late for " + inst)
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
        if (d != null && d.nonEmpty) {
          payload.writeInt(d.size)
          payload.writeBytes(d)
        } else {
          payload.writeInt(emp.size)
          payload.writeBytes(emp)
        }
        rt.sendMessage(dest, Tag(inst,0,Decision,0), payload)
        Logger("Runner", Debug, s"$id sending decision to ${dest.id} for $inst")
      case None =>
        val payload = PooledByteBufAllocator.DEFAULT.buffer()
        payload.writeLong(8)
        rt.sendMessage(dest, Tag(inst,0,Late,0), payload)
        //TODO send the whole state
        Logger("Runner", Debug, s"$id sending late to ${dest.id} for $inst")
    }
  }
  
  def shutdown: Long = {
    rt.shutdown
    decisionProcessor.interrupt
    requestsProcessor.interrupt
    decisionProcessor.join
    requestsProcessor.join
    if (log != null) {
      log.close
    }
    nbr
  }

  /** to force the JIT load everything, start a dummy decision */
  def warmupJIT: Unit = {
    val dummy = Array.ofDim[Byte](1024)
    val io = new ConsensusIO[Array[Byte]] {
      val initialValue = dummy
      def decide(value: Array[Byte]): Unit = { }
    }
    Thread.sleep(1000)
    for (i <- 0 until 10) {
      alg.startInstance(i.toShort, io, Set.empty)
    }
    //let it run for a while
    Thread.sleep(options.delay - 2000)
    for (i <- 0 until 10) {
      alg.stopInstance(i.toShort)
    }
    Thread.sleep(1000)
    lck.lock
    try {
      jitting = false
    } finally {
      lck.unlock
    }
  }


  def start: Unit = {
    rt.startService
    requestsProcessor.start
    decisionProcessor.start

    //if you comment out this method, make sure to replace it by `jitting = false`
    warmupJIT
 
    // eager case
    if (!isLeader && options.eagerStart) {
      lck.lock
      try {
        for (i <- 0 until options.rate) {
          val inst = nextInstance
          startInstance(inst, emp, Set.empty)
        }
      } finally {
        lck.unlock
      }
    }
  }

}


object Runner extends RTOptions {
  
  final val Decision = 4
  final val Late = 5
  final val ForwardedBatch = 6
  final val AskDecision = 7

  var logFile: Option[String] = None
  newOption("--log", dzufferey.arg.String(str => logFile = Some(str) ), "log file prefix")

  var eagerStart = false
  newOption("--eager", dzufferey.arg.Unit( () => eagerStart = true), "start the instances on the replica eagerly (default: lazily)")
  newOption("-e", dzufferey.arg.Unit( () => eagerStart = true), "start the instances on the replica eagerly (default: lazily)")

  var rate = 1
  newOption("--rate", dzufferey.arg.Int( i => rate = i), "fix the rate, i.e., #queries in parallel (default: 1)")
  newOption("-r", dzufferey.arg.Int( i => rate = i), "fix the rate, i.e., #queries in parallel (default: 1)")

  var n = 50
  newOption("-n", dzufferey.arg.Int( i => n = i), "number of different values that we can modify")

  var delay = 4000
  newOption("--delay", dzufferey.arg.Int( i => delay = i), "delay in ms before making queries (allow the replicas to setup)")
  newOption("-d", dzufferey.arg.Int( i => delay = i), "delay in ms before making queries (allow the replicas to setup)")

  var batchSize = 100
  newOption("--batch", dzufferey.arg.Int( i => batchSize = i), "batch size")
  newOption("-b", dzufferey.arg.Int( i => batchSize = i), "batch size")

  var late = 10
  newOption("--late", dzufferey.arg.Int( i => late = i), "fix the late coefficient (default: 10)")
  newOption("-l", dzufferey.arg.Int( i => late = i), "fix the late coefficient (default: 10)")

  var pending = 5
  newOption("--pending", dzufferey.arg.Int( i => pending = i), "fix the pending requests coefficient (default: 5)")
  newOption("-p", dzufferey.arg.Int( i => pending = i), "fix the pending requests coefficient (default: 5)")

  var dpTO = 5
  newOption("--dpTO", dzufferey.arg.Int( i => dpTO = i), "DecisionProcessor Timeout (default: 5)")

  var rpTO = 1
  newOption("--rpTO", dzufferey.arg.Int( i => rpTO = i), "RequestProcessor Timeout (default: 1)")

  var cr = 200
  newOption("--cr", dzufferey.arg.Int( i => cr = i), "how many requests to simulate at once (default: 200)")

  var forward = true
  newOption("--noForwarding", dzufferey.arg.Unit( () => forward = false), "disable forwarding (batches of) requests to the leader")

  var sync = false
  newOption("--sync", dzufferey.arg.Unit( () => sync = false), "enable PessimisticByzantineSynchronizer")

  var shortTO = 5L
  newOption("--shortTO", dzufferey.arg.Int( i => shortTO = i), "shortTO (default: 5)")

  val usage = "..."
  
  var begin = 0L

  var system: Runner = null 

  def main(args: Array[java.lang.String]): Unit = {
    apply(args.toIndexedSeq)
    system = new Runner(this)
    system.start // this take a while (JIT and stuff)
    val prng = new util.Random()

    Logger("Runner", Notice, s"$id, starting")
    begin = java.lang.System.currentTimeMillis()

    //TODO many clients (only if leader or forward)
    if (forward || id == 0) {
      while (true) {
        var i = 0
        while (i < cr) {
          system.propose(id, prng.nextInt(n), prng.nextInt())
          i += 1
        }
        Thread.sleep(1)
      }
    } else {
      while (true) {
        Thread.sleep(1000)
      }
    }

  }
  
  java.lang.Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run(): Unit = {
        val versionNbr = system.shutdown
        val end = java.lang.System.currentTimeMillis()
        val duration = (end - begin) / 1000
        Logger("Runner", Notice, id.toString + ", #decisions = " + versionNbr + ", Δt = " + duration + ", throughput = " + (versionNbr/duration))
      }
    }
  )

}
