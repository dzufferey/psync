package example

import round._
import round.runtime._
import round.macros.Macros._

class FloodMinProcess(f: Int) extends Process[ConsensusIO] {
  
  var x = 0
  var callback: ConsensusIO = null

  def init(io: ConsensusIO) {
    callback = io
    x = io.initialValue
  }

  val rounds = phase(
    new Round[Int]{
    
      def send: Map[ProcessID,Int] = {
        broadcast( x )
      }

      def update(mailbox: Map[ProcessID,Int]) {
        x = mailbox.foldLeft(x)( (acc, v) => math.min(acc, v._2) )
        if (r > f) {
          callback.decide(x)
          terminate()
        }
      }

    }
  )

}

class FloodMin(f: Int) extends Algorithm[ConsensusIO,FloodMinProcess] {

  val spec = TrivialSpec //TODO we need safety predicates on transition to account for synchronous crash-stop

  def process = new FloodMinProcess(f)

  def dummyIO = new ConsensusIO{
    val initialValue = 0
    def decide(value: Int) { }
  }
}

object FloodMinRunner extends RTOptions {
  
  var f = 2
  newOption("-f", dzufferey.arg.Int( i => f = i), "f (default = 2)")

  var confFile = "src/test/resources/3replicas-conf.xml"
  
  val usage = "..."
  
  var rt: Runtime[ConsensusIO,FloodMinProcess] = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[java.lang.String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    val alg = new FloodMin(f)
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
