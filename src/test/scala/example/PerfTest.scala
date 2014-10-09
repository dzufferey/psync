package example

import round._
import round.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.arg._
import java.util.concurrent.Semaphore
import scala.util.Random

object PerfTest extends Options {
  
  
  newOption("-v", Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", Unit(() => Logger.lessVerbose), "decrease the verbosity level.")

  var id = -1
  newOption("-id", Int( i => id = i), "the replica ID")

  var confFile = "src/test/resources/sample-conf.xml"
  newOption("--conf", String(str => confFile = str ), "config file")
  
  var lv = false
  newOption("-lv", Unit( () => lv = true), "use the last voting instead of the OTR")
  
  var rate = new Semaphore(10)
  newOption("-rt", Int( i => rate = new Semaphore(i)), "fix the rate (how many instance in parallel)")

  var rd = new Random()
  newOption("-r", Int( i => rd = new Random(i)), "random number generator seed")
  
  var to = 50
  newOption("-to", Int( i => to = i), "timeout")

  val usage = "..."
  
  var begin = 0l
  var versionNbr = 0l

  var rt: RunTime[ConsensusIO] = null

  def defaultHandler(msg: Message) { msg.release }

  def main(args: Array[java.lang.String]) {
    apply(args)
    val alg = if (lv) new LastVoting()
              else new OTR2()
    rt = new RunTime(alg)
    rt.startService(defaultHandler(_), confFile, Map("id" -> id.toString, "timeout" -> to.toString))
    Thread.sleep(1000)
    begin = java.lang.System.currentTimeMillis()
    while (true) {
      rate.acquire
      versionNbr = versionNbr + 1
      val r = rd.nextInt()
      Logger("PerfTest", Info, versionNbr.toString + "\t  starting with value " + r)
      val io = new ConsensusIO {
        val v = versionNbr 
        val initialValue = r
        def decide(value: scala.Int) {
          Logger("PerfTest", Info, v.toString + "\t  decision is " + value)
          rate.release
        }
      }
      rt.startInstance(versionNbr.toShort, io)
    }
  }
  
  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        rt.shutdown
        val end = java.lang.System.currentTimeMillis()
        val duration = (end - begin) / 1000
        println("#instances = " + versionNbr + ", Δt = " + duration + ", throughput = " + (versionNbr/duration))
      }
    }
  )

}
