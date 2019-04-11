package example

import psync._
import psync.runtime._
import psync.macros.Macros._
import psync.utils.serialization._

// https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-852j-distributed-algorithms-fall-2009/lecture-notes/MIT6_852JF09_lec24.pdf (page 23)

class SelfStabilizingMutualExclusionProcess extends Process[Unit] {

  var x = 0
  var neighbour = new ProcessID(-1)

  def init(nope: Unit) = i{
    x = scala.util.Random.nextInt() //start in an arbitrary state
    neighbour = new ProcessID( ((id.id + 1) % n).toShort ) //right neighbour
    println("replica " + id.id + " has neighbour " + neighbour.id)
  }

  val rounds = phase(
    new EventRound[Int]{

      def init = Progress.timeout(SelfStabilizingRunner.timeout)
    
      def send: Map[ProcessID,Int] = Map(neighbour -> x)

      def receive(sender: ProcessID, xNeighbour: Int) = {
        //println("replica: "+id.id+" has x = " + x + " at round " + r.toInt) 
        if (id.id == 0) {
          if (x == xNeighbour) {
            println("replica: "+id.id+" has the token for round " + r.toInt) 
            x = (x+1) % (n+1)
          }
        } else {
          if (x != xNeighbour) {
            println("replica: "+id.id+" has the token for round " + r.toInt) 
            x = xNeighbour
          }
        }
        Progress.goAhead
      }

    }
  )
}

class SelfStabilizingMutualExclusion(rt: Runtime) extends Algorithm[Unit,SelfStabilizingMutualExclusionProcess](rt) {
  
  val spec = TrivialSpec

  def process = new SelfStabilizingMutualExclusionProcess

  def dummyIO = ()

}

object SelfStabilizingRunner extends RTOptions {

  var confFile = "src/test/resources/25replicas-conf.xml"
  
  val usage = "..."
  
  var rt: Runtime = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[java.lang.String]) {
    val start = java.lang.System.currentTimeMillis()
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    rt = new Runtime(this, defaultHandler(_))
    rt.startService
    val alg = new SelfStabilizingMutualExclusion(rt)

    val cur = java.lang.System.currentTimeMillis()
    //Thread.sleep(8000 + start - cur) //time to run all the processes
    alg.startInstance(0, ())
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
