package example

import round._
import round.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.util.concurrent.Semaphore
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.ConcurrentLinkedQueue
import scala.util.Random

//TODO add recovery a la PerfTest, but make sure we always have a decision!

class PerfTest2(id: Int,
                confFile: String,
                nbrValues: Short,
                _rate: Short,
                logFile: Option[String],
                additionalOptions: Map[String,String]
               )
{

  val rate = new Semaphore(_rate)

  val log: java.io.BufferedWriter =
    if (logFile.isDefined) new java.io.BufferedWriter(new java.io.FileWriter(logFile.get + "_" + id + ".log"))
    else null
  val lck = new ReentrantLock

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

  val nbr = new AtomicLong(0l)

  def defaultHandler(msg: Message) {
    val value = msg.getInt(0)
    val idx = (value >>> 16).toShort
    val v2 = backOff(idx).poll
    val v = if (v2 != 0) v2 else (value & 0xFFFF).toShort
    start(idx, v, v2 != 0, Set(msg))
  }

  def start(idx: Short, value: Short, self: Boolean, msg: Set[Message]) {
    if (running(idx).tryAcquire) {
      var instanceNbr = (versions(idx) + nbrValues).toShort
      //in case of msg check that we have the right instance!
      if (!msg.isEmpty) {
        val m = msg.head
        val inst = m.instance
        if (self || Instance.leq(instanceNbr, inst)) {
          //take the largest of inst/instanceNbr
          instanceNbr = Instance.max(instanceNbr, inst)
        } else {
          //that message came late, drop it
          m.release
          running(idx).release()
          return
        }
      }
      versions(idx) = instanceNbr
      val io = new ConsensusIO {
        val initialValue = (idx << 16) | (value & 0xFFFF)
        def decide(value: scala.Int) {
          val idx = (value >>> 16).toShort
          val v = (value & 0xFFFF).toShort
          //save result
          values(idx) = v
          //releases resources
          running(idx).release()
          if (self) rate.release
          //log
          nbr.incrementAndGet
          if (log != null) {
            lck.lock
            try {
              log.write(idx.toString + "\t" + instanceNbr + "\t" + v)
              log.newLine()
            } finally {
              lck.unlock
            }
          }
          //check for pending request
          val b = backOff(idx).poll
          if (b != 0) start(idx, b, true, Set())
        }
      }
      rt.startInstance(instanceNbr, io, msg)
    } else {
      //if an instance is already running push if to the backoff queue if it is one of our own query
      if (self) {
        backOff(idx).add(value)
      }
      msg.foreach(_.release)
    }
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
