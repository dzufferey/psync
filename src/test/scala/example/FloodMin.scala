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

  val spec = TrivialSpec

  def process = p(new Process[ConsensusIO]{

    def init(io: ConsensusIO) {
      callback <~ io
      x <~ io.initialValue
    }

    val rounds = Array[Round](
      rnd(new Round{
      
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

      })
    )
  })
}

object FloodMinRunner extends round.utils.DefaultOptions {
  
  var id = -1
  newOption("-id", dzufferey.arg.Int( i => id = i), "the replica ID")

  var f = 2
  newOption("-f", dzufferey.arg.Int( i => f = i), "f (default = 2)")

  var confFile = "src/test/resources/3replicas-conf.xml"
  newOption("--conf", dzufferey.arg.String(str => confFile = str ), "config file")
  
  val usage = "..."
  
  var rt: RunTime[ConsensusIO] = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[java.lang.String]) {
    apply(args)
    val alg = new FloodMin(f)
    rt = new RunTime(alg)
    rt.startService(defaultHandler(_), confFile, Map("id" -> id.toString))

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
