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

class PerfTest2(options: RuntimeOptions,
                nbrValues: Short,
                _rate: Short,
                algorithm: String,
                logFile: Option[String],
                additionalOptions: Map[String,String]
               ) extends DecisionLog[Int]
{

  final val Decision = 4
  final val Recovery = 5

  val id = options.id
  val rate = new Semaphore(_rate)

  val log: java.io.BufferedWriter =
    if (logFile.isDefined) new java.io.BufferedWriter(new java.io.FileWriter(logFile.get + "_" + id + ".log"))
    else null
  val logLock = new ReentrantLock

  if (log != null) {
    log.write("idx\tinst\tval")
    log.newLine()
  }

  val alg = ConsensusSelector(algorithm, additionalOptions)
  val rt = new RunTime(alg, options, defaultHandler(_))
  rt.startService

  val values   = Array.ofDim[Short](nbrValues)
  val versions = Array.ofDim[Short](nbrValues)
  val running  = Array.ofDim[Option[Short]](nbrValues)
  val backOff  = Array.ofDim[ConcurrentLinkedQueue[Short]](nbrValues)
  for (i <- 0 until nbrValues) {
    running(i) = None
    backOff(i) = new ConcurrentLinkedQueue[Short]()
    versions(i) = i.toShort
  }
  def lock(idx: Short) = decisionLocks(decIdx(idx))
  val nbr = new AtomicLong(0l)
  val selfStarted = new ConcurrentSkipListSet[Short]()

  def defaultHandler(msg: Message) {
    val flag = msg.tag.flag
    //Logger("PerfTest", Debug, "defaultHandler: " + msg.instance)

    //might need to start a new instance:
    // initial values is either taken from the backOff queue or the message
    if (flag == Flags.normal || flag == Flags.dummy) {
      val value = msg.getInt(0)
      val idx = (value >>> 16).toShort
      //println("(0) id: " + id + " idx: " + idx + ", instance: " + msg.instance + ", round:" + msg.round)
      val v2 = backOff(idx).poll
      val v = if (v2 != 0) v2 else (value & 0xFFFF).toShort
      start(idx, v, v2 != 0, Set(msg))

    } else if (flag == Decision) {
      val inst = msg.instance
      //Logger("PerfTest", Info, inst + " got a decision message")
      val value = msg.getInt(0)
      val idx = (value >>> 16).toShort
      //println("(1) id: " + id + " idx: " + idx + ", instance: " + inst)
      val first = processDecision(inst, value)
      msg.release
      rt.stopInstance(inst)
      if (first) checkPending(idx) 

    } else if (flag == Recovery) {
      val inst = msg.instance
      val value = msg.getInt(0)
      val idx = (value >>> 16).toShort
      val newInstance = msg.getInt(4).toShort
      //println("(2) id: " + id + " idx: " + idx + ", instance: " + inst + ", newInstance: " + newInstance)
      //Logger("PerfTest", Info, inst + " recovery to " + newInstance)
      assert((inst - newInstance).toShort % nbrValues == 0, "inst = " + inst + ", newInst = " + newInstance)
      val first = processDecision(inst, value, Some(newInstance)) 
      msg.release
      rt.stopInstance(inst)
      if (first) checkPending(idx) 

    } else {
       sys.error("unknown or error flag: " + flag)
    }
  }

  def processDecision(instance: Short, value: Int, recovery: Option[Short] = None) = { 
    var firstTime = false
    val idx = (value >>> 16).toShort
    val v = (value & 0xFFFF).toShort
    assert(v != 0,  "instance: " + instance + ", idx " + idx + ", value " + v)
    val l = lock(idx)
    var myInst: Short = 0
    l.lock
    try {
      myInst = recovery match {
        case Some(v) =>
          assert(Instance.leq(instance, v), instance.toString + ", " + v)
          v
        case None =>
          instance
      }
      running(idx) match {
        case Some(ran) =>
          if (Instance.leq(ran, myInst)) {
            //Logger("PerfTest", Info, myInst + " decide: " + idx + ", " + v)
            pushDecision(myInst, value)
            versions(idx) = myInst
            values(idx) = v
            firstTime = true
            //releases resources
            running(idx) = None
          }
        case None =>
      }
    } finally {
      l.unlock
    }

    if (firstTime) {
      if (selfStarted contains instance) {
        rate.release()
        selfStarted.remove(instance)
        //Logger("PerfTest", Info, instance + "     selfStarted")
      } else {
        //Logger("PerfTest", Info, instance + " not selfStarted")
      }
      
      nbr.incrementAndGet

      //log
      if (log != null) {
        logLock.lock
        try {
          log.write(idx.toString + "\t" + myInst + "\t" + v)
          log.newLine()
        } finally {
          logLock.unlock
        }
      }
    }
    firstTime
  }
  
  /** send either the decision if it is still on the log, or the currrent value and version */
  def sendRecoveryInfo(m: Message) = {
    val inst = m.instance
    val idx = (m.getInt(0) >>> 16).toShort
    val payload = PooledByteBufAllocator.DEFAULT.buffer()
    val sender = m.senderId
    payload.writeLong(8)
    var tag = Tag(0,0)
    getDec(inst) match {
      case Some(d) =>
        //Logger("PerfTest", Info, "sending decision " + (d >>> 16) + ", " + (d & 0xFFFF).toShort +
        //                         " to " + sender.id + " for instance " + inst)
        tag = Tag(inst,0,Decision,0)
        payload.writeInt(d)
      case None =>
        val l = lock(idx)
        l.lock
        try {
          var currInst = versions(idx)
          val value = values(idx)
          val d = (idx << 16) | (value.toInt & 0xFFFF)
          if (currInst == inst) {
            //Logger("PerfTest", Info, "sending decision " + (d >>> 16) + ", " + (d & 0xFFFF).toShort +
            //                         " to " + sender.id + " for instance " + inst)
            tag = Tag(inst,0,Decision,0)
            payload.writeInt(d)
          } else {
            //Logger("PerfTest", Info, "sending recovery " + (d >>> 16) + ", " + (d & 0xFFFF).toShort +
            //                         " to " + sender.id + " for instance " + inst + " -> " + currInst)
            tag = Tag(inst,0,Recovery,0)
            payload.writeInt(d)
            payload.writeInt(currInst)
          }
        } finally {
          l.unlock
        }
    }
    rt.sendMessage(sender, tag, payload)
  }

  /** */
  def start(idx: Short, value: Short, self: Boolean, _msg: Set[Message]) {
    var canGo = false
    var instanceNbr: Short = 0
    var msg = _msg

    val l = lock(idx)
    l.lock
    try {
        
      instanceNbr = versions(idx)

      //recovery
      msg = msg.filter( m => {
        val inst = m.instance
        if (Instance.leq(inst, instanceNbr)) {
          sendRecoveryInfo(m)
          m.release
          false
        } else {
          true
        }
      })

      instanceNbr = (instanceNbr + nbrValues).toShort

      if (running(idx).isEmpty) {

        //in case of msg check that we have the right instance!
        if (!msg.isEmpty) {
          assert(msg.size == 1)
          val m = msg.head
          val inst = m.instance
          if (Instance.lt(inst, instanceNbr)) {
            m.release
            msg = Set()
          } else {
            instanceNbr = Instance.max(instanceNbr, inst)
            canGo = true
          }
        }

        canGo = canGo || self

        if (canGo) {
          //good to go
          running(idx) = Some(instanceNbr)
        }

      }

    } finally {
      l.unlock
    }

    if (canGo) {
      val v = (idx << 16) | (value & 0xFFFF)
      val io = new ConsensusIO {
        val initialValue = v
        //TODO we should reduce the amount of work done here: pass it to another thread and let the algorithm thread continue.
        def decide(value: Int) {
          //Logger("PerfTest", Notice, "(" + id + ") normal decision: instanceNbr " +  instanceNbr + ", value: " + value)
          val first = processDecision(instanceNbr, value)
          if (first) {
            rt.submitTask( () => checkPending(idx) )
          }
        }
      }
      if (self) {
        selfStarted add instanceNbr
      }
      //Logger("PerfTest", Notice, "(" + id + ") starting instance " + instanceNbr + " with " + idx + ", " + value + ", " + v + ", self " + self)
      rt.startInstance(instanceNbr, io, msg)
      if (self) wakeupOthers(instanceNbr, v)

    } else {
      //Logger("PerfTest", Debug, "backing off " + idx)
      //an instance is already running push the request to the backoff queue if it is one of our own query
      if (self) {
        backOff(idx).add(value)
      }
      for (m <- msg) {
        m.release
      }
    }
  }
  
  def wakeupOthers(inst: Short, initValue: Int) {
    //TODO better way
    if (algorithm == "lv" || algorithm == "slv") {
      val dir = rt.directory
      for (o <- dir.others) {
        val payload = PooledByteBufAllocator.DEFAULT.buffer()
        payload.writeLong(8)
        var tag = Tag(inst,0,Flags.dummy,0)
        payload.writeInt(initValue)
        rt.sendMessage(o.id, tag, payload)
      }
    }
  }

  def checkPending(idx: Short) {
    val b = backOff(idx).poll
    if (b != 0) start(idx, b, true, Set())
  }

  def propose(idx: Short, value: Short) {
    rate.acquire
    start(idx, value, true, Set())
  }

  def shutdown: Long = {
    rt.shutdown
    if (log != null) {
      log.close
    }
    nbr.get
  }

}

object PerfTest2 extends RTOptions {

  var confFile = "src/test/resources/sample-conf.xml"
  
  var logFile: Option[String] = None
  newOption("--log", dzufferey.arg.String(str => logFile = Some(str) ), "log file prefix")

  var n = 50
  newOption("-n", dzufferey.arg.Int( i => n = i), "number of different values that we can modify")

  var rate = 10
  newOption("-rt", dzufferey.arg.Int( i => rate = i), "fix the rate (#queries in parallel)")

  var rd = new Random()
  newOption("-r", dzufferey.arg.Int( i => rd = new Random(i)), "random number generator seed")
  
  var algorithm = ""
  newOption("-lv", dzufferey.arg.Unit( () => algorithm = "lv"), "use the last voting algorithm")
  newOption("-a", dzufferey.arg.String( a => algorithm = a), "use the given algorithm (otr, lv, slv)")
  
  var after = -1
  newOption("-after", dzufferey.arg.Int( i => after = i), "#round after decision")

  var delay = 1000
  newOption("-delay", dzufferey.arg.Int( i => delay = i), "delay in ms before making queries (allow the replicas to setup)")

  val usage = "..."
  
  var begin = 0l

  var system: PerfTest2 = null 

  def main(args: Array[java.lang.String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    val opts =
      if (after >= 0) Map("after" -> after.toString)
      else Map[String, String]()
    system = new PerfTest2(this, n.toShort, rate.toShort, algorithm, logFile, opts)

    //let the system setup before starting
    Thread.sleep(delay)
    begin = java.lang.System.currentTimeMillis()

    //makes queries ...
    while (true) {
      val slot = rd.nextInt(n).toShort
      var value = (rd.nextInt(32766) + 1).toShort
      system.propose(slot, value)
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
