package example

import round._
import round.runtime._
import round.utils.ByteBufAllocator
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.arg._
import java.util.concurrent.Semaphore
import java.util.concurrent.locks.ReentrantLock
import scala.util.Random

object PerfTest extends Options with DecisionLog[scala.Int] {
  
  
  newOption("-v", Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", Unit(() => Logger.lessVerbose), "decrease the verbosity level.")

  var id = -1
  newOption("-id", Int( i => id = i), "the replica ID")

  var confFile = "src/test/resources/sample-conf.xml"
  newOption("--conf", String(str => confFile = str ), "config file")
  
  var logFile = ""
  newOption("--log", String(str => logFile = str ), "log file prefix")

  var lv = false
  newOption("-lv", Unit( () => lv = true), "use the last voting instead of the OTR")
  
  var rate = new Semaphore(10)
  newOption("-rt", Int( i => rate = new Semaphore(i)), "fix the rate (how many instance in parallel)")

  var rd = new Random()
  newOption("-r", Int( i => rd = new Random(i)), "random number generator seed")
  
  var to = 50
  newOption("-to", Int( i => to = i), "timeout")
  
  var after = 2
  newOption("-after", Int( i => after = i), "#round after decision")

  val usage = "..."
  
  var begin = 0l
  var versionNbr = 0l

  var rt: RunTime[ConsensusIO] = null

  final val Decision = 3
  final val TooLate = 4
  
  def defaultHandler(msg: Message) {
    val inst = msg.instance
    if ((inst - versionNbr.toShort).toShort < 0) { //with wrapping
      val l = decisionLocks(decIdx(inst))
      l.lock
      try {
        val flag = msg.tag.flag
        if (flag == Flags.normal || flag == Flags.dummy) {
          trySendDecision(inst, msg.senderId)
        } else if (flag == Decision) {
          Logger("PerfTest", Info, inst + " got decision! (" + versionNbr + ")")
          onDecision(inst, -1, msg.getInt(0))
          rt.stopInstance(inst)
        } else if (flag == TooLate) {
          Logger("PerfTest", Warning, inst + " too late! (" + versionNbr + ")")
          rt.stopInstance(inst)
        } else {
          sys.error("unknown or error flag: " + flag)
        }
      } finally {
        l.unlock
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
            log.write("instance " + inst + "\tver " + versionNbr + "\tdecision " + value)
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
          
  def trySendDecision(inst: Short, senderId: ProcessID) = {
    Logger("PerfTest", Info, inst + " sending recovery to " + senderId)
    val payload = ByteBufAllocator.buffer(16)
    payload.writeLong(8)
    val tag = getDec(inst) match {
      case Some(d) =>
        payload.writeInt(d)
        Tag(inst,0,Decision,0)
      case None =>
        Tag(inst,0,TooLate,0)
    }
    rt.sendMessage(senderId, tag, payload)
  }

  val lck = new ReentrantLock 
  var log: java.io.BufferedWriter = null

  def main(args: Array[java.lang.String]) {
    apply(args)
    if (logFile != "") {
      val fw = new java.io.FileWriter(logFile + "_" + id + ".log")
      log = new java.io.BufferedWriter(fw)
    } 
    val alg = if (lv) new LastVoting()
              else new OTR2(after)
    rt = new RunTime(alg)
    rt.startService(defaultHandler(_), confFile, Map("id" -> id.toString, "timeout" -> to.toString))
    Thread.sleep(1000)
    begin = java.lang.System.currentTimeMillis()
    while (true) {
      rate.acquire
      val next = versionNbr + 1
      val r = rd.nextInt()
      Logger("PerfTest", Info, next.toString + "\t  starting with value " + r)
      val io = new ConsensusIO {
        val inst = next.toShort
        val v = next
        val initialValue = r
        def decide(value: scala.Int) { onDecision(inst, v, value) }
      }
      rt.startInstance(next.toShort, io)
      versionNbr = next
    }
  }
  
  Runtime.getRuntime().addShutdownHook(
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
