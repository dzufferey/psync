package example

import round._
import round.runtime._
import round.utils.ByteBufAllocator
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.util.concurrent.Semaphore
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.ConcurrentSkipListSet
import scala.util.Random

//TODO add recovery a la PerfTest, but make sure we always have a decision!

class PerfTest2(id: Int,
                confFile: String,
                nbrValues: Short,
                _rate: Short,
                logFile: Option[String],
                additionalOptions: Map[String,String]
               ) extends DecisionLog[Int]
{

  final val Decision = 4
  final val Recovery = 5

  val rate = new Semaphore(_rate)

  val log: java.io.BufferedWriter =
    if (logFile.isDefined) new java.io.BufferedWriter(new java.io.FileWriter(logFile.get + "_" + id + ".log"))
    else null
  val logLock = new ReentrantLock

  if (log != null) {
    log.write("idx\tinst\tval")
    log.newLine()
  }

  val alg =  new OTR2()
  val rt = new RunTime(alg)
  rt.startService(defaultHandler(_), confFile, additionalOptions + ("id" -> id.toString))

  val values   = Array.ofDim[Short](nbrValues)
  val versions = Array.ofDim[Short](nbrValues)
  val running  = Array.ofDim[Semaphore](nbrValues)
  val backOff  = Array.ofDim[ConcurrentLinkedQueue[Short]](nbrValues)
  for (i <- 0 until nbrValues) {
    running(i) = new Semaphore(1)
    backOff(i) = new ConcurrentLinkedQueue[Short]()
    versions(i) = i.toShort
  }
  def lock(idx: Short) = decisionLocks(decIdx(idx))
  val nbr = new AtomicLong(0l)
  val selfStarted = new ConcurrentSkipListSet[Short]()

  def defaultHandler(msg: Message) {
    val flag = msg.tag.flag

    //might need to start a new instance:
    // initial values is either taken from the backOff queue or the message
    if (flag == Flags.normal || flag == Flags.dummy) {
      val value = msg.getInt(0)
      val idx = (value >>> 16).toShort
      val v2 = backOff(idx).poll
      val v = if (v2 != 0) v2 else (value & 0xFFFF).toShort
      start(idx, v, v2 != 0, Set(msg))

    } else if (flag == Decision) {
      val inst = msg.instance
      Logger("PerfTest", Info, inst + " got a decision message")
      val value = msg.getInt(0)
      val idx = (value >>> 16).toShort
      val first = processDecision(inst, value)
      msg.release
      rt.stopInstance(inst)
      if (first) checkPending(idx) 

    } else if (flag == Recovery) {
      val inst = msg.instance
      val value = msg.getInt(0)
      val idx = (value >>> 16).toShort
      val newInstance = msg.getInt(4).toShort
      Logger("PerfTest", Info, inst + " recovery to " + newInstance)
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
    val l = lock(idx)
    var myInst: Short = 0
    l.lock
    try {
      val current = versions(idx)
      myInst = recovery match {
        case Some(v) =>
          //assert(Instance.lt(instance, v), instance.toString + ", " + v)
          v
        case None =>
          instance
      }
      if (!Instance.lt(myInst, current)) {
        if (running(idx).availablePermits() == 0) {
          Logger("PerfTest", Info, instance + " decide: " + idx + ", " + v)
          pushDecision(myInst, value)
          versions(idx) = myInst
          values(idx) = v
          firstTime = true
          //releases resources
          running(idx).release()
        }
      }
    } finally {
      l.unlock
    }

    if (firstTime) {
      if (selfStarted contains instance) {
        rate.release
        selfStarted.remove(instance)
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
    val payload = ByteBufAllocator.buffer(16)
    val sender = m.senderId
    payload.writeLong(8)
    var tag = Tag(0,0)
    getDec(inst) match {
      case Some(d) =>
        Logger("PerfTest", Info, "sending decision to " + sender.id)
        tag = Tag(inst,0,Decision,0)
        payload.writeInt(d)
      case None =>
        val l = lock(idx)
        l.lock
        try {
          val currInst = versions(idx)
          val value = values(idx)
          val d = (idx << 16) | (value.toInt & 0xFFFF)
          if (currInst == inst) {
            Logger("PerfTest", Info, "sending decision to " + sender.id)
            tag = Tag(inst,0,Decision,0)
            payload.writeInt(d)
          } else {
            Logger("PerfTest", Info, "sending recovery to " + sender.id)
            tag = Tag(inst,0,Recovery,0)
            payload.writeInt(d)
            payload.writeInt(currInst)
          }
        } finally {
          l.unlock
        }
    }
    rt.sendMessage(m.senderId, tag, payload)
  }

  /** */
  def start(idx: Short, value: Short, self: Boolean, msg: Set[Message]) {
    if (running(idx).tryAcquire) {

      var instanceNbr: Short = 0

      val l = lock(idx)
      l.lock
      try {

        instanceNbr = (versions(idx) + nbrValues).toShort
       
        //in case of msg check that we have the right instance!
        if (!msg.isEmpty) {
          assert(msg.size == 1)
          val m = msg.head
          val inst = m.instance
          if (Instance.leq(instanceNbr, inst)) {
            //take the largest of inst/instanceNbr
            instanceNbr = inst
          } else {
            //that message came late, send recovery info 
            instanceNbr = Instance.max(instanceNbr, inst)
            sendRecoveryInfo(m)
            //if we don't have a pending value, stop here
            if (!self) {
              running(idx).release()
              m.release
              return
            }
          }
        }
       
        //good to go
        versions(idx) = instanceNbr
      } finally {
        l.unlock
      }

      val io = new ConsensusIO {
        val initialValue = (idx << 16) | (value & 0xFFFF)
        def decide(value: Int) {
          val first = processDecision(instanceNbr, value)
          if (first) checkPending(idx)
        }
      }
      if (self) {
        selfStarted add instanceNbr
      }
      rt.startInstance(instanceNbr, io, msg)

    } else {
      Logger("PerfTest", Debug, "backing off " + idx)
      //an instance is already running push the request to the backoff queue if it is one of our own query
      if (self) {
        backOff(idx).add(value)
      }
      for (m <- msg) {
        var instanceNbr = versions(idx)
        val inst = m.instance
        if (Instance.lt(inst, instanceNbr)) {
          sendRecoveryInfo(m)
        }
        m.release
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

object PerfTest2 extends dzufferey.arg.Options {

  newOption("-v", dzufferey.arg.Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", dzufferey.arg.Unit(() => Logger.lessVerbose), "decrease the verbosity level.")

  var id = -1
  newOption("-id", dzufferey.arg.Int( i => id = i), "the replica ID")

  var confFile = "src/test/resources/sample-conf.xml"
  newOption("--conf", dzufferey.arg.String(str => confFile = str ), "config file")
  
  var logFile: Option[String] = None
  newOption("--log", dzufferey.arg.String(str => logFile = Some(str) ), "log file prefix")

  var n = 50
  newOption("-n", dzufferey.arg.Int( i => n = i), "number of different values that we can modify")

  var rate = 10
  newOption("-rt", dzufferey.arg.Int( i => rate = i), "fix the rate (#queries in parallel)")

  var rd = new Random()
  newOption("-r", dzufferey.arg.Int( i => rd = new Random(i)), "random number generator seed")
  
  var to = 50
  newOption("-to", dzufferey.arg.Int( i => to = i), "timeout")

  val usage = "..."
  
  var begin = 0l

  var system: PerfTest2 = null 

  def main(args: Array[java.lang.String]) {
    apply(args)
    val opts = Map("timeout" -> to.toString)
    system = new PerfTest2(id, confFile, n.toShort, rate.toShort, logFile, opts)

    //let the system setup before starting
    Thread.sleep(1000)
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
