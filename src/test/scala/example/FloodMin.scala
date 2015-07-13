package example

import round._
import round.runtime._
import round.macros.Macros._

class FloodMin(f: Int) extends Algorithm[ConsensusIO] {

  import VarHelper._
  import SpecHelper._

  val x = new LocalVariable[Int](0)
  //
  val callback = new LocalVariable[ConsensusIO](null)

  val spec = TrivialSpec //TODO we need safety predicates on transition to account for synchronous crash-stop

  def process = p(new Process[ConsensusIO]{

    def init(io: ConsensusIO) {
      callback <~ io
      x <~ io.initialValue
    }

    val rounds = phase(
      new Round{
      
        type A = Int

        def send: Set[(Int,ProcessID)] = {
          broadcast( x )
        }

        def update(mailbox: Set[(Int, ProcessID)]) {
          x <~ mailbox.foldLeft(x: Int)( (acc, v) => math.min(acc, v._1) )
          if (r > f) {
            callback.decide(x)
            terminate()
          }
        }

      }
    )
  })
}

object FloodMinRunner extends RTOptions {
  
  var f = 2
  newOption("-f", dzufferey.arg.Int( i => f = i), "f (default = 2)")

  var confFile = "src/test/resources/3replicas-conf.xml"
  
  val usage = "..."
  
  var rt: RunTime[ConsensusIO] = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[java.lang.String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    val alg = new FloodMin(f)
    rt = new RunTime(alg, this, defaultHandler(_))
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
