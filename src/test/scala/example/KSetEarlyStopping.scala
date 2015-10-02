package example

import round._
import round.runtime._
import round.macros.Macros._

//http://link.springer.com/chapter/10.1007%2F11535294_5
class KSetESProcess(t: Int, k: Int) extends Process[ConsensusIO] {

  var est = 0
  var canDecide = false
  var lastNb = 0
  var callback: ConsensusIO = null

  def init(io: ConsensusIO) = i{
    canDecide = false
    lastNb = n
    est = io.initialValue
    callback = io
  }

  val rounds = phase(
    new Round[(Int, Boolean)]{
    
      def send: Map[ProcessID,(Int, Boolean)] = {
        broadcast( (est: Int) -> (canDecide: Boolean) )
      }

      def update(mailbox: Map[ProcessID,(Int, Boolean)]) {
        val currNb = mailbox.size
        if (r > t/k || canDecide) {
          callback.decide(est)
          terminate
        } else {
          est = mailbox.map(_._2._1).min
          canDecide = (mailbox.exists(_._2._2) || lastNb - currNb < k)
          lastNb = currNb
        }
      }

    }
  )
}


class KSetEarlyStopping(t: Int, k: Int) extends Algorithm[ConsensusIO,KSetESProcess] {
  
  val spec = TrivialSpec

  def process = new KSetESProcess(t, k)

}

object KSetESRunner extends RTOptions {
  
  var k = 2
  newOption("-k", dzufferey.arg.Int( i => k = i), "k (default = 2)")

  var t = 2
  newOption("-t", dzufferey.arg.Int( i => k = i), "t (default = 2)")

  var confFile = "src/test/resources/3replicas-conf.xml"
  
  val usage = "..."
  
  var rt: Runtime[ConsensusIO,KSetESProcess] = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[java.lang.String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    val alg = new KSetEarlyStopping(t, k)
    rt = new Runtime(alg, this, defaultHandler(_))
    rt.startService

    import scala.util.Random
    val init = Random.nextInt
    val io = new ConsensusIO {
      val initialValue = init
      def decide(value: Int) {
        Console.println("replica " + id + " decided " + value)
      }
    }
    Thread.sleep(100)
    Console.println("replica " + id + " starting with " + init)
    rt.startInstance(0, io)
  }
  
  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        rt.shutdown
      }
    }
  )
}
