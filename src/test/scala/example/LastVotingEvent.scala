package example

import psync._
import psync.Time._
import psync.formula._
import psync.macros.Macros._
import psync.runtime.Runtime
import psync.utils.serialization._


class LastVotingEvent(rt: Runtime, timeout: Long) extends Algorithm[ConsensusIO[Int], LVEProcess](rt) {

  import SpecHelper._

  val spec = TrivialSpec
  
  def process = new LVEProcess(timeout)
  
  def dummyIO = new ConsensusIO[Int]{
    val initialValue = 0
    def decide(value: Int) { }
  }
}
  
class LVEProcess(timeout: Long) extends Process[ConsensusIO[Int]]{

  //variables
  var x = 0
  var ts = new Time(-1)
  var ready = false
  var commit = false
  var vote = 0
  var decision = -1 //TODO as ghost
  var decided = false
  //
  var callback: ConsensusIO[Int] = null

      
  def coord: ProcessID = new ProcessID((r / 4 % n).toShort)
    
  def init(io: ConsensusIO[Int]) {
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

    //TODO can be EventRound[Unit] as we just confirm
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
