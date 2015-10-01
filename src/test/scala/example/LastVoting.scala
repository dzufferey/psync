package example

import round._
import round.Time._
import round.formula._
import round.macros.Macros._

class LastVoting extends Algorithm[ConsensusIO, LVProcess] {

  import SpecHelper._

  val V = new Domain[Int]
  
  def coord(phi: Int): LVProcess = sys.error("for spec only")

  val spec = new Spec {
    val livenessPredicate = List[Formula](
      P.exists( p => P.forall( q => p == coord(r/4) && p.HO.contains(q) && p.HO.size > n/2 ) )
    )

    val noDecision: Formula = P.forall( i => !i.decided && !i.ready)

    val majority: Formula =
      V.exists( v => V.exists( t => {
          val A = P.filter( i => i.ts >= t )
          A.size > n/2 &&
          t <= r/4 &&
          P.forall( i => (A.contains(i) ==> (i.x == v) ) &&
                         (i.decided ==> (i.decision == v) ) &&
                         (i.commit ==> (i.vote == v) ) &&
                         (i.ready ==> (i.vote == v) ) &&
                         ((i.ts == r/4) ==> coord(r/4).commit ))
      }) )

    val keepInit: Formula = P.forall( i => P.exists( j1 => i.x == init(j1.x) ))

    val safetyInv = And(keepInit, Or(noDecision, majority))

    val invariants = List[Formula](
      safetyInv,
      P.exists( j => P.forall( i => i.decided && i.decision == init(j.x)) )
    )
    
    override val roundInvariants = List(
      List[Formula](
        true,
        P.exists( i => i.commit )
      ),
      List[Formula](
        true,
        P.exists( i => i.commit && P.forall( j => j.ts == r/4 && j.x == i.vote ))
      ),
      List[Formula](
        true,
        P.exists( i => i.commit && i.ready && P.forall( j => j.ts == r/4 && j.x == i.vote ))
      )
    )

    val properties = List[(String,Formula)](
      ("Termination",    P.forall( i => i.decided) ),
      ("Agreement",      P.forall( i => P.forall( j => (i.decided && j.decided) ==> (i.decision == j.decision) ))),
      ("Validity",       P.forall( i => i.decided ==> P.exists( j => init(j.x) == i.decision ))),
      ("Integrity",      P.exists( j => P.forall( i => i.decided ==> (i.decision == init(j.x)) ))),
      ("Irrevocability", P.forall( i => old(i.decided) ==> (i.decided && old(i.decision) == i.decision) ))
    )
  }
  
  def process = new LVProcess()
}
  
class LVProcess extends Process[ConsensusIO]{

  //variables
  var x = 0
  var ts = new Time(-1)
  var ready = false
  var commit = false
  var vote = 0
  var decision = -1 //TODO as ghost
  var decided = false
  //
  var callback: ConsensusIO = null

      
  def coord(phi: Int): ProcessID = new ProcessID((phi % n).toShort)
    
  def init(io: ConsensusIO) {
    callback = io
    x = io.initialValue
    ts = -1
    decided = false 
    ready = false
    commit = false
  }

  val rounds = phase(
    new Round[(Int,Time)]{

      def send(): Map[ProcessID,(Int, Time)] = {
        Map(coord(r/4) -> (x, ts))
      }

      override def expectedNbrMessages = if (id == coord(r/4)) n/2 + 1 else 0

      def update(mailbox: Map[ProcessID,(Int, Time)]) {
        assert(r.toInt % 4 == 0)
        if (id == coord(r/4) && mailbox.size > n/2) {
          // let θ be one of the largest θ from 〈ν, θ〉received
          // vote(p) := one ν such that 〈ν, θ〉 is received
          vote = mailbox.maxBy(_._2._2)._2._1
          commit = true
          assert(vote != 0, mailbox.mkString(", "))
        }
      }

    },

    new Round[Int]{

      def send(): Map[ProcessID,Int] = {
        if (id == coord(r/4) && commit) {
          broadcast(vote)
        } else {
          Map.empty
        }
      }

      override def expectedNbrMessages = 1

      def update(mailbox: Map[ProcessID,Int]) {
        if (mailbox contains coord(r/4)) {
          x = mailbox(coord(r/4))
          ts = r/4
          assert(x != 0)
        }
      }

    },

    new Round[Int]{

      def send(): Map[ProcessID,Int] = {
        if ( ts == (r/4) ) {
          Map( coord(r/4) -> x )
        } else {
          Map.empty
        }
      }

      override def expectedNbrMessages = if (id == coord(r/4)) n/2 + 1 else 0

      def update(mailbox: Map[ProcessID,Int]) {
        if (id == coord(r/4) && mailbox.size > n/2) {
          ready = true
        }
      }

    },

    new Round[Int]{

      def send(): Map[ProcessID, Int] = {
        if (id == coord(r/4) && ready) {
          broadcast(vote)
        } else {
          Map.empty
        }
      }

      override def expectedNbrMessages = 1 

      def update(mailbox: Map[ProcessID,Int]) {
        if (mailbox contains coord(r/4)) {
          val v = mailbox(coord(r/4))
          assert(v != 0)
          callback.decide(v)
          decision = v
          decided = true
          exitAtEndOfRound()
        }
        ready = false
        commit = false
      }

    }

  )

}
