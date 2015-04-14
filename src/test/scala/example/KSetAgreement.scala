package example

import round._
import round.runtime._
import round.macros.Macros._

class KSetAgreement(i: ProcessID, k: Int) extends Algorithm[ConsensusIO] {
  
  import VarHelper._
  import SpecHelper._

  
  val t = new LocalVariable[Map[ProcessID,Int]](Map.empty[ProcessID,Int])
  val decider = new LocalVariable[Boolean](false)
  //
  val callback = new LocalVariable[ConsensusIO](null)

  val spec = TrivialSpec
  //k-agreement: the set Y of decision values is such that Y ⊆ V₀ ∧ |Y| ≤ k
  //uncertainty: there exists a (k+1)-valent initial configuration

  //model, assumptions:
  // n > 2(k-1)
  // crash-fault, f < k
  // completely async (no termination requirement)

  def process = p(new Process[ConsensusIO]{

    def init(io: ConsensusIO) {
      callback <~ io
      decider <~ false
      //FIXME: crash in the scala compiler (because of id)
      //t <~ Map(id -> io.initialValue)
      t <~ Map(i -> io.initialValue)
    }

    val rounds = Array[Round](
      rnd(new Round{
      
        type A = (Boolean, Map[ProcessID,Int])

        def merge(a: Map[ProcessID,Int], b: Map[ProcessID,Int]) = {
          a ++ b
        }

        def pick(a: Map[ProcessID,Int]) = a.values.min

        def send: Set[((Boolean, Map[ProcessID,Int]),ProcessID)] = {
          broadcast( (decider: Boolean) -> (t: Map[ProcessID,Int]) )
        }

        def update(mailbox: Set[((Boolean, Map[ProcessID,Int]), ProcessID)]) {
          val content = mailbox.map(_._1)
          if (decider) {
            callback.decide(pick(t))
            terminate()
          } else if (content.exists(_._1)) {
            decider <~ true
            t <~ content.find(_._1).get._2
          } else {
            val same = mailbox.filter(_._1._2 == (t: Map[ProcessID, Int]))
            if (same.size > n - k) {
              decider <~ true
            } else {
              for (((_,v),_) <- mailbox)
                t <~ merge(t, v)
            }
          }
        }

      })
    )
  })
}

object KSetRunner extends RTOptions {
  
  var k = 2
  newOption("-k", dzufferey.arg.Int( i => k = i), "k (default = 2)")

  var confFile = "src/test/resources/3replicas-conf.xml"
  
  val usage = "..."
  
  var rt: RunTime[ConsensusIO] = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[java.lang.String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    val alg = new KSetAgreement(new ProcessID(id.toShort), k)
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
