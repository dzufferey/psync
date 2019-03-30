package example

import psync._
import psync.runtime._
import psync.macros.Macros._
import psync.utils.serialization._

// http://www.cs.yale.edu/homes/aspnes/pinewiki/FailureDetectors.html

class EsfdProcess(period: Long, hysteresis: Int) extends Process[Unit] {

  val lastSeen = scala.collection.mutable.Map[ProcessID,Int]()

  def getSuspected = {
    lastSeen.foldLeft(Set[ProcessID]())( (acc, kv) => {
      val (pid, last) = kv
      if (last > hysteresis) acc + pid else acc
    })
  }
  
  def init(nope: Unit) = i{
    for (i <- 0 until n) {
      lastSeen(new ProcessID(i.toShort)) = 0
    }
  }

  val rounds = phase(
    new EventRound[Set[ProcessID]]{

      def init = {
        for ( (k,v) <- lastSeen ) {
          lastSeen(k) = math.min(v + 1, hysteresis + 1) //avoid overflow
        }
        Progress.timeout(period)
      }
    
      def send: Map[ProcessID,Set[ProcessID]] = {
        broadcast(getSuspected)
      }

      def receive(sender: ProcessID, suspected: Set[ProcessID]) = {
        lastSeen(sender) = 0
        for (s <- suspected) {
          if (lastSeen(s) != 0) { //not heard from s this round
            lastSeen(s) = hysteresis + 1
          }
        }
        Progress.unchanged
      }

      override def finishRound(didTimeout: Boolean) = {
        println("replica: "+id.id+" suspecting: " + getSuspected.map(_.id).mkString(", ")) 
        true
      }
    }
  )
}

class Esfd(period: Long, hysteresis: Int) extends Algorithm[Unit,EsfdProcess] {
  
  val spec = TrivialSpec

  def process = new EsfdProcess(period, hysteresis)

  def dummyIO = ()
}

object EsfdRunner extends RTOptions {

  var period = 1000l
  newOption("--period", dzufferey.arg.Long( i => period = i), "(default = 1000)")
  var hysteresis = 5
  newOption("--hysteresis", dzufferey.arg.Int( i => hysteresis = i), "(default = 5)")

  var confFile = "src/test/resources/25replicas-conf.xml"
  
  val usage = "..."
  
  var rt: Runtime[Unit,EsfdProcess] = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[java.lang.String]) {
    val start = java.lang.System.currentTimeMillis()
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    val alg = new Esfd(period, hysteresis)
    rt = new Runtime(alg, this, defaultHandler(_))
    rt.startService

    val cur = java.lang.System.currentTimeMillis()
    Thread.sleep(math.max(8000 + start - cur, 0)) //time to run all the processes
    rt.startInstance(0, ())
    // idle while the algorithms runs
    while (true) Thread.sleep(100000)
  }
  
  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        rt.shutdown
      }
    }
  )

}
