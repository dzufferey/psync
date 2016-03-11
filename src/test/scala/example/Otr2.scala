package example

import psync._
import psync.formula._
import psync.macros.Macros._

class Otr2Process(afterDecision: Int) extends Process[ConsensusIO]{
  
  var x = 0
  var decision: Option[Int] = None //as ghost
  var after = afterDecision
  var callback: ConsensusIO = null

  def init(io: ConsensusIO) = i{
    callback = io
    x = io.initialValue
    decision = None
    after = afterDecision
  }

  val rounds = phase(
    new Round[Int]{

      //min most often received
      def mmor(mailbox: Map[ProcessID,Int]): Int = {
        val byValue = mailbox.groupBy(_._2)
        import scala.math.Ordered._
        val m = byValue.minBy{ case (v, procs) => (-procs.size.toLong, v) }
        m._1
      } ensuring { v1 =>
        mailbox.forall{ case (k, v2) =>
          mailbox.count{ case (k, v3) => v1 == v3 } > mailbox.count{ case (k, v3) => v2 == v3 } || v1 <= v2
        }
      }

      def send(): Map[ProcessID,Int] = {
        broadcast(x) //macro for (x, All)
      }

      def update(mailbox: Map[ProcessID,Int]) {
        if (mailbox.size > 2*n/3) {
          val v = mmor(mailbox)
          x = v
          if (mailbox.count{ case (k, msg) => msg == v } > 2*n/3) {
            if (decision.isEmpty) {
              callback.decide(v)
            }
            decision = Some(v);
          }
        }
        if (decision.isDefined) {
          after = after - 1
          if(after <= 0) {
            terminate()
          }
        }
      }

    }
  )

}

//a version of OTR that eventually terminates
class OTR2(afterDecision: Int = 2) extends Algorithm[ConsensusIO,Otr2Process] {

  import SpecHelper._

  val V = new Domain[Int]

  val spec = new Spec {
      val livenessPredicate = List[Formula]( S.exists( s => P.forall( p => p.HO == s && s.size > 2*n/3 )))
      val invariants = List[Formula](
           P.forall( i => !i.decision.isEmpty )
        || V.exists( v => {
             val A = P.filter( i => i.x == v);
             A.size > 2*n/3 && P.forall( i => i.decision.isDefined ==> (i.decision.get == v))
           }),
        V.exists( v => {
          val A = P.filter( i => i.x == v);
          A.size == n && P.forall( i => i.decision.isDefined ==> (i.decision.get == v))
        }),
        V.exists( v => P.forall( i => i.decision.isDefined ==> (i.decision.get == v)) )
      ) //how to relate the invariants and the magic rounds

    val properties = List[(String,Formula)](
      ("Termination",    P.forall( i => i.decision.isDefined) ),
      ("Agreement",      P.forall( i => P.forall( j => (i.decision.isDefined && j.decision.isDefined) ==> (i.decision == j.decision) ))),
      ("Validity",       P.forall( i => i.decision.isDefined ==> P.exists( j => init(j.x) == i.decision.get ))),
      ("Integrity",      P.exists( j => P.forall( i => i.decision.isDefined ==> (i.decision.get == init(j.x)) ))),
      ("Irrevocability", P.forall( i => old(i.decision).isDefined ==> (old(i.decision) == i.decision) ))
    )
  }
  
  
  def process = new Otr2Process(afterDecision)

  def dummyIO = new ConsensusIO{
    val initialValue = 0
    def decide(value: Int) { }
  }
}
