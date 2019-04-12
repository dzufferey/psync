package example

import psync._
import psync.runtime._
import psync.utils.ByteBufAllocator
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.util.concurrent.Semaphore
import java.util.concurrent.locks.ReentrantLock
import scala.util.Random

object PerfTest extends RTOptions with DecisionLog[scala.Int] {
  
  
  var confFile = "src/test/resources/sample-conf.xml"
  
  var logFile = ""
  newOption("--log", dzufferey.arg.String(str => logFile = str ), "log file prefix")

  var algorithm = ""
  newOption("-lv", dzufferey.arg.Unit( () => algorithm = "lv"), "use the last voting algorithm")
  newOption("-a", dzufferey.arg.String( a => algorithm = a), "use the given algorithm (otr, lv, slv)")
  
  var rate = new Semaphore(10)
  newOption("-rt",  dzufferey.arg.Int( i => rate = new Semaphore(i)), "fix the rate (how many instance in parallel)")

  var rd = new Random()
  newOption("-r",  dzufferey.arg.Int( i => rd = new Random(i)), "random number generator seed")
  
  val usage = "..."
  
  var begin = 0l
  var versionNbr = 0l

  var rt: Runtime = null
  var alg: Algorithm[ConsensusIO[Int],_] = null

  final val Decision = 3
  final val TooLate = 4
  
  def defaultHandler(msg: Message) {
    val inst = msg.instance
    if (Instance.leq(inst, versionNbr.toShort)) {
      val flag = msg.tag.flag
      if (flag == Flags.normal || flag == Flags.dummy) {
        if (Instance.lt(inst, versionNbr.toShort)) {
          trySendDecision(inst, msg.sender)
        }
      } else if (flag == Decision) {
        Logger("PerfTest", Info, inst + " got decision! (" + versionNbr + ")")
        onDecision(inst, -1, msg.getInt(0))
        alg.stopInstance(inst)
      } else if (flag == TooLate) {
        Logger("PerfTest", Warning, inst + " too late! (" + versionNbr + ")")
        alg.stopInstance(inst)
      } else {
        sys.error("unknown or error flag: " + flag)
      }
    }
    msg.release
  }

  def onDecision(inst: Short, versionNbr: Long, value: scala.Int) {
    val l = decisionLocks(decIdx(inst))
    l.lock
    try {
      if (getDec(inst).isEmpty) {
        pushDecision(inst, value)
        rate.release
        Logger("PerfTest", Info, "instance " + inst + "\tver " + versionNbr + "\tdecision " + value)
        if (log != null) {
          lck.lock
          try {
            log.write(inst + "\t" + versionNbr + "\t" + value)
            log.newLine()
          } finally {
            lck.unlock
          }
        }
      }
    } finally {
      l.unlock
    }
  }
          
  def trySendDecision(inst: Short, sender: ProcessID) = {
    Logger("PerfTest", Info, inst + " sending recovery to " + sender)
    val payload = ByteBufAllocator.buffer(16)
    payload.writeLong(8)
    val tag = getDec(inst) match {
      case Some(d) =>
        payload.writeInt(d)
        Tag(inst,0,Decision,0)
      case None =>
        Tag(inst,0,TooLate,0)
    }
    rt.sendMessage(sender, tag, payload)
  }

  val lck = new ReentrantLock 
  var log: java.io.BufferedWriter = null

  def main(args: Array[java.lang.String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    if (logFile != "") {
      val fw = new java.io.FileWriter(logFile + "_" + id + ".log")
      log = new java.io.BufferedWriter(fw)
    } 
    rt = Runtime(this, defaultHandler)
    rt.startService
    alg = ConsensusSelector(algorithm, rt, Map())
    Thread.sleep(1000)
    begin = java.lang.System.currentTimeMillis()
    while (true) {
      rate.acquire
      val next = versionNbr + 1
      val r = rd.nextInt()
      Logger("PerfTest", Info, next.toString + "\t  starting with value " + r)
      val io = new ConsensusIO[Int] {
        val inst = next.toShort
        val v = next
        val initialValue = r
        def decide(value: scala.Int) { onDecision(inst, v, value) }
      }
      alg.startInstance(next.toShort, io)
      versionNbr = next
    }
  }
  
  java.lang.Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        rt.shutdown
        val end = java.lang.System.currentTimeMillis()
        val duration = (end - begin) / 1000
        println("#instances = " + versionNbr + ", Δt = " + duration + ", throughput = " + (versionNbr/duration))
        if (log != null) {
          log.close
        }
      }
    }
  )

}
