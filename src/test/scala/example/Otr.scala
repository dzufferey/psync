package example

import psync._
import psync.formula._
import psync.macros.Macros._
import psync.verification.{requires,ensures}

//like OTR but uses a boolean flag instead of an option for the decision


class OtrProcess(afterDecision: Int) extends Process[ConsensusIO]{

  var x = 0
  var decision = -1 //as ghost
  var decided = false
  var after = afterDecision
  var callback: ConsensusIO = null
    
  def init(io: ConsensusIO) = i{
    callback = io
    x = io.initialValue
    decided = false
    after = afterDecision
  }

  //min most often received
  @requires(True())
//@ensures("v1", {
//  import psync.logic.CL.procType
//  import InlineOps._
//  val v1 = Variable("v1").setType(Int)
//  val v2 = Variable("v2").setType(Int)
//  val p1 = Variable("p1").setType(procType)
//  val mb = Variable("mailbox").setType(FMap(procType, Int))
//  val cv1 = Comprehension(List(p1), mb.isDefinedAt(p1) && mb.lookUp(p1) === v1).card
//  val cv2 = Comprehension(List(p1), mb.isDefinedAt(p1) && mb.lookUp(p1) === v2).card
//  ForAll(List(v2), And(
//    cv2 <= cv1,
//    Implies(cv1 === cv2, v1 <= v2)
//  ))
//} )
  def mmor(mailbox: Map[ProcessID,Int]): Int = {
    val byValue = mailbox.groupBy(_._2)
    import scala.math.Ordered._
    val m = byValue.minBy{ case (v, procs) => (-procs.size, v) }
    m._1
  }
  /*ensuring { v1 =>
    mailbox.forall{ case (k, v2) =>
      mailbox.filter(_._2 == v1).size > mailbox.filter(_._2 == v2).size || v1 <= v2
    }
  }*/

  val rounds = phase(
    new Round[Int]{

      def send(): Map[ProcessID,Int] = {
        broadcast(x) //macro for (x, All)
      }

      def update(mailbox: Map[ProcessID,Int]) {
        if (mailbox.size > 2*n/3) {
          val v = mmor(mailbox)
          x = v
          if (mailbox.filter{ case (k, msg) => msg == v }.size > 2*n/3) {
            if (!decided) {
              callback.decide(v)
            }
            decided = true
            decision = v
          }
        }
        if (decided) {
          after = after - 1
          if(after <= 0) {
            //terminate()
            exitAtEndOfRound
          }
        }
      }

    }
  )

}


class OTR(afterDecision: Int = 2) extends Algorithm[ConsensusIO, OtrProcess] {

  import SpecHelper._

  val V = new Domain[Int]

  val spec = new Spec {
    val goodRound: Formula = S.exists( s => P.forall( p => p.HO == s && s.size > 2*n/3 ))
    val livenessPredicate = List( goodRound, goodRound )
    val invariants = List[Formula](
      (    P.forall( i => !i.decided )
        || V.exists( v => {
             val A = P.filter( i => i.x == v);
             A.size > 2*n/3 && P.forall( i => i.decided ==> (i.decision == v))
           })
      ) && P.forall( i => P.exists( j1 => i.x == init(j1.x) )),
         V.exists( v => {
            val A = P.filter( i => i.x == v);
            A.size == (n: Int) && P.forall( i => i.decided ==> (i.decision == v))
         })
      && P.forall( i => P.exists( j1 => i.x == init(j1.x) )),
      P.exists( j => P.forall( i => i.decided && i.decision == init(j.x)) )
    )

    val properties = List[(String,Formula)](
      ("Termination",    P.forall( i => i.decided) ),
      ("Agreement",      P.forall( i => P.forall( j => (i.decided && j.decided) ==> (i.decision == j.decision) ))),
      ("Validity",       P.forall( i => i.decided ==> P.exists( j => init(j.x) == i.decision ))),
      ("Integrity",      P.exists( j => P.forall( i => i.decided ==> (i.decision == init(j.x)) ))),
      ("Irrevocability", P.forall( i => old(i.decided) ==> (i.decided && old(i.decision) == i.decision) ))
    )
  }
  
  def process = new OtrProcess(afterDecision)

  def dummyIO = new ConsensusIO{
    val initialValue = 0
    def decide(value: Int) { }
  }
}
