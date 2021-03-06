package example

import psync._
import psync.runtime._
import psync.macros.Macros._
import psync.utils.serialization._

object KSetAgreementSerialization {
  implicit val reg = new KryoRegistration[(Boolean,Map[ProcessID,Int])] {
    val mapSerializer = new CollectionSerializer[(ProcessID,Int), Map[ProcessID,Int]]
    override def registerClasses = Seq(classOf[Tuple2[_,_]])
    override def registerClassesWithSerializer = Seq(
      classOf[ProcessID] -> new ProcessIDSerializer,
      classOf[Map[ProcessID,Int]] -> mapSerializer
    )
  }
}

import KSetAgreementSerialization.reg

class KSetProcess(k: Int, timeout: Long) extends Process[ConsensusIO[Int]] {
  
  var t = Map.empty[ProcessID,Int]
  var decider = false
  var callback: ConsensusIO[Int] = null

  def init(io: ConsensusIO[Int]): Unit = {
    callback = io
    decider = false
    t = Map(id -> io.initialValue)
  }
    
  val rounds = phase(
    new Round[(Boolean, Map[ProcessID,Int])](timeout){
    
      def merge(a: Map[ProcessID,Int], b: Map[ProcessID,Int]) = {
        a ++ b
      }

      def pick(a: Map[ProcessID,Int]) = a.values.min

      def send: Map[ProcessID,(Boolean, Map[ProcessID,Int])] = {
        broadcast( decider -> t )
      }

      def update(mailbox: Map[ProcessID,(Boolean, Map[ProcessID,Int])]): Unit = {
        val content = mailbox.map{ case (k,v) => v }
        if (decider) {
          callback.decide(pick(t))
          exitAtEndOfRound()
        } else if (content.exists(_._1)) {
          decider = true
          t = content.find(_._1).get._2
        } else {
          val same = mailbox.filter(_._2._2 == t)
          if (same.size > n - k) {
            decider = true
          } else {
            for ((_,(_,v)) <- mailbox)
              t = merge(t, v)
          }
        }
      }

    }
  )

}

class KSetAgreement(rt: Runtime, k: Int, timeout: Long) extends Algorithm[ConsensusIO[Int],KSetProcess](rt) {
  
  val spec = TrivialSpec
  //k-agreement: the set Y of decision values is such that Y ⊆ V₀ ∧ |Y| ≤ k
  //uncertainty: there exists a (k+1)-valent initial configuration

  //model, assumptions:
  // n > 2(k-1)
  // crash-fault, f < k
  // completely async (no termination requirement)

  def process = new KSetProcess(k, timeout)

  def dummyIO = new ConsensusIO[Int]{
    val initialValue = 0
    def decide(value: Int): Unit = { }
  }
}

object KSetRunner extends Runner {
  
  var k = 2
  newOption("-k", dzufferey.arg.Int( i => k = i), "k (default = 2)")

  def onStart: Unit = {
    val alg = new KSetAgreement(rt, k, timeout)

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
