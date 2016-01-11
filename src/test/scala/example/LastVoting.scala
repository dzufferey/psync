package example

import psync._
import psync.Time._
import psync.formula._
import psync.macros.Macros._

class LastVoting extends Algorithm[ConsensusIO, LVProcess] {

  import SpecHelper._

  val V = new Domain[Int]
  
  def coord: LVProcess = sys.error("for spec only")

  val spec = new Spec {
    val livenessPredicate = List[Formula](
      P.exists( p => P.forall( q => p == coord && p.HO.contains(q) && p.HO.size > n/2 ) )
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
                         ((i.ts == r/4) ==> coord.commit ))
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
  
  def dummyIO = new ConsensusIO{
    val initialValue = 0
    def decide(value: Int) { }
  }
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

      
  def coord: ProcessID = new ProcessID((r / 4 % n).toShort)
    
  def init(io: ConsensusIO) {
    callback = io
    x = io.initialValue
    ts = -1
    decided = false 
    ready = false
    commit = false
  }

  val rounds = phase(
    //FIXME use Int until PerfTest2 is fixed
    //new Round[(Int,Time)]{
    new Round[(Int,Int)]{

      //def send(): Map[ProcessID,(Int, Time)] = {
      def send(): Map[ProcessID,(Int, Int)] = {
        Map(coord -> (x, ts.toInt))
      }

      override def expectedNbrMessages = if (id == coord) n/2 + 1 else 0

      //def update(mailbox: Map[ProcessID,(Int, Time)]) {
      def update(mailbox: Map[ProcessID,(Int, Int)]) {
        assert(r.toInt % 4 == 0)
        if (id == coord && mailbox.size > n/2) {
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
        if (id == coord && commit) {
          broadcast(vote)
        } else {
          Map.empty
        }
      }

      override def expectedNbrMessages = 1

      def update(mailbox: Map[ProcessID,Int]) {
        if (mailbox contains coord) {
          x = mailbox(coord)
          ts = r/4
          assert(x != 0)
        }
      }

    },

    new Round[Int]{

      def send(): Map[ProcessID,Int] = {
        if ( ts == (r/4) ) {
          Map( coord -> x )
        } else {
          Map.empty
        }
      }

      override def expectedNbrMessages = if (id == coord) n/2 + 1 else 0

      def update(mailbox: Map[ProcessID,Int]) {
        if (id == coord && mailbox.size > n/2) {
          ready = true
        }
      }

    },

    new Round[Int]{

      def send(): Map[ProcessID, Int] = {
        if (id == coord && ready) {
          broadcast(vote)
        } else {
          Map.empty
        }
      }

      override def expectedNbrMessages = 1 

      def update(mailbox: Map[ProcessID,Int]) {
        if (mailbox contains coord) {
          val v = mailbox(coord)
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
