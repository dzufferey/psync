package example

import psync._
import psync.runtime._
import psync.macros.Macros._
import psync.utils.serialization._

class FloodMinProcess(f: Int, timeout: Long) extends Process[ConsensusIO[Int]] {
  
  var x = 0
  var callback: ConsensusIO[Int] = null

  def init(io: ConsensusIO[Int]) {
    callback = io
    x = io.initialValue
  }

  val rounds = phase(
    new Round[Int](timeout){
    
      def send: Map[ProcessID,Int] = {
        broadcast( x )
      }

      def update(mailbox: Map[ProcessID,Int]) {
        x = mailbox.foldLeft(x)( (acc, v) => math.min(acc, v._2) )
        if (r > f) {
          callback.decide(x)
          exitAtEndOfRound()
        }
      }

    }
  )

}

class FloodMin(rt: Runtime, f: Int, timeout: Long) extends Algorithm[ConsensusIO[Int],FloodMinProcess](rt) {

  val spec = TrivialSpec //TODO we need safety predicates on transition to account for synchronous crash-stop

  def process = new FloodMinProcess(f, timeout)

  def dummyIO = new ConsensusIO[Int]{
    val initialValue = 0
    def decide(value: Int) { }
  }
}

object FloodMinRunner extends Runner {
  
  var f = 2
  newOption("-f", dzufferey.arg.Int( i => f = i), "f (default = 2)")

  def onStart {
    val alg = new FloodMin(rt, f, timeout)

    import scala.util.Random
    val init = Random.nextInt
    val io = new ConsensusIO[Int] {
      val initialValue = init
      def decide(value: Int) {
        Console.println("replica " + id + " decided " + value)
      }
    }
    Thread.sleep(100)
    Console.println("replica " + id + " starting with " + init)
    alg.startInstance(0, io)
  }
  
}
