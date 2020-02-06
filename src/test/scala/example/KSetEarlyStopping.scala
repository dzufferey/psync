package example

import psync._
import psync.runtime._
import psync.macros.Macros._
import psync.utils.serialization._

//http://link.springer.com/chapter/10.1007%2F11535294_5
class KSetESProcess(t: Int, k: Int, timeout: Long) extends Process[ConsensusIO[Int]] {

  var est = 0
  var canDecide = false
  var lastNb = 0
  var callback: ConsensusIO[Int] = null

  def init(io: ConsensusIO[Int]) = i{
    canDecide = false
    lastNb = n
    est = io.initialValue
    callback = io
  }

  val rounds = phase(
    new Round[(Int, Boolean)](timeout){
    
      def send: Map[ProcessID,(Int, Boolean)] = {
        broadcast( (est: Int) -> (canDecide: Boolean) )
      }

      def update(mailbox: Map[ProcessID,(Int, Boolean)]): Unit = {
        val currNb = mailbox.size
        if (r > t/k || canDecide) {
          callback.decide(est)
          exitAtEndOfRound
        } else {
          est = mailbox.map(_._2._1).min
          canDecide = (mailbox.exists(_._2._2) || lastNb - currNb < k)
          lastNb = currNb
        }
      }

    }
  )
}


class KSetEarlyStopping(rt: Runtime, t: Int, k: Int, timeout: Long) extends Algorithm[ConsensusIO[Int],KSetESProcess](rt) {
  
  val spec = TrivialSpec

  def process = new KSetESProcess(t, k, timeout)

  def dummyIO = new ConsensusIO[Int]{
    val initialValue = 0
    def decide(value: Int): Unit = { }
  }
}

object KSetESRunner extends Runner {
  
  var k = 2
  newOption("-k", dzufferey.arg.Int( i => k = i), "k (default = 2)")

  var t = 2
  newOption("-t", dzufferey.arg.Int( i => k = i), "t (default = 2)")

  
  def onStart: Unit = {
    val alg = new KSetEarlyStopping(rt, t, k, timeout)

    import scala.util.Random
    val init = Random.nextInt
    val io = new ConsensusIO[Int] {
      val initialValue = init
      def decide(value: Int): Unit = {
        Console.println("replica " + id + " decided " + value)
      }
    }
    Thread.sleep(100)
    Console.println("replica " + id + " starting with " + init)
    alg.startInstance(0, io)
  }
}
