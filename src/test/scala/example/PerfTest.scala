package example

import round._
import round.runtime._
import round.utils.{Logger, Arg, Options}
import round.utils.LogLevel._
import java.util.concurrent.Semaphore
import scala.util.Random

object PerfTest extends Options {
  
  
  newOption("-v", Arg.Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", Arg.Unit(() => Logger.lessVerbose), "decrease the verbosity level.")

  var id = -1
  newOption("-id", Arg.Int( i => id = i), "the replica ID")

  var confFile = "src/test/resources/sample-conf.xml"
  newOption("--conf", Arg.String(str => confFile = str ), "config file")
  
  var lv = false
  newOption("-lv", Arg.Unit( () => lv = true), "use the last voting instead of the OTR")
  
  var rate = new Semaphore(10)
  newOption("-rt", Arg.Int( i => rate = new Semaphore(i)), "use the last voting instead of the OTR")

  var rd = new Random()
  newOption("-r", Arg.Int( i => rd = new Random(i)), "random number generator seed")
  
  var to = 50
  newOption("-to", Arg.Int( i => to = i), "timeout")

  val usage = "..."
  
  var begin = 0l
  var versionNbr = 0l

  var rt: RunTime[ConsensusIO] = null

  def defaultHandler(msg: Message) { msg.release }

  def main(args: Array[String]) {
    apply(args)
    val alg = if (lv) new LastVoting()
              else new OTR2()
    rt = new RunTime(alg)
    rt.startService(defaultHandler, confFile, Map("id" -> id.toString, "timeout" -> to.toString))
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
        def decide(value: Int) {
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
