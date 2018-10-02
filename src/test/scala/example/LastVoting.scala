package example

import psync._
import psync.Time._
import psync.formula._
import psync.macros.Macros._
import psync.utils.serialization._
import psync.runtime.Progress

class LastVoting(timeout: Long) extends Algorithm[ConsensusIO, LVProcess] {

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
          r > 0 &&
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
  
  def process = new LVProcess(timeout)
  
  def dummyIO = new ConsensusIO{
    val initialValue = 0
    def decide(value: Int) { }
  }
}
  
class LVProcess(timeout: Long) extends Process[ConsensusIO]{

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
    new EventRound[(Int,Time)]{

      var nMsg = 0
      var maxTime = new Time(-1)
      var maxVal = 0

      def init = {
        nMsg = 0
        maxTime = new Time(-1)
        maxVal = x
        if (r == new Time(0) || id != coord) {
          Progress.goAhead
        } else {
          Progress.timeout( timeout )
        }
      }

      def send(): Map[ProcessID,(Int, Time)] = {
        if (r == new Time(0)) {
          Map.empty
        } else {
          Map(coord -> (x, ts))
        }
      }

      def receive(sender: ProcessID, payload: (Int, Time)) = {
        nMsg += 1
        if (payload._2 >= maxTime) {
          maxTime = payload._2
          maxVal = payload._1
        }
        if (nMsg > n/2) Progress.goAhead
        else Progress.unchanged
      }
      
      override def finishRound(didTimeout: Boolean) = {
        if (id == coord && !didTimeout) {
          commit = true
          vote = maxVal
          assert(vote != 0)
        }
        true
      }

    },

    new EventRound[Int]{

      def init = {
        if (id == coord && !commit) Progress.goAhead
        else Progress.timeout( timeout )
      }

      def send(): Map[ProcessID,Int] = {
        if (id == coord && commit) {
          broadcast(vote)
        } else {
          Map.empty[ProcessID,Int]
        }
      }

      def receive(sender: ProcessID, payload: Int) = {
        if (sender == coord) {
          x = payload
          ts = r/4
          assert(x != 0)
          Progress.goAhead
        } else {
          Progress.unchanged
        }
      }

    },

    new EventRound[Int]{

      var nMsg = 0

      def init = {
        nMsg = 0
        if (id == coord) {
          Progress.timeout( timeout )
        } else {
          Progress.goAhead
        }
      }

      def send(): Map[ProcessID,Int] = {
        if ( ts == (r/4) ) {
          Map( coord -> x )
        } else {
          Map.empty[ProcessID,Int]
        }
      }

      def receive(sender: ProcessID, payload: Int) = {
        nMsg += 1
        if (nMsg > n/2) Progress.goAhead
        else Progress.unchanged
      }

      override def finishRound(didTimeout: Boolean) = {
        ready = (!didTimeout && id == coord)
        true
      }

    },

    new EventRound[Int]{

      def init = {
        if (id == coord && !ready) Progress.goAhead
        else Progress.timeout( timeout )
      }

      def send(): Map[ProcessID, Int] = {
        if (id == coord && ready) {
          broadcast(vote)
        } else {
          Map.empty[ProcessID,Int]
        }
      }

      def receive(sender: ProcessID, payload: Int) = {
        if (sender == coord) {
          assert(payload != 0)
          decision = payload
          decided = true
          Progress.goAhead
        } else {
          Progress.unchanged
        }
      }

      override def finishRound(didTimeout: Boolean) = {
        ready = false
        commit = false
        if (decided) {
          callback.decide(decision)
          false
        } else {
          true
        }
      }

    }

  )

}
