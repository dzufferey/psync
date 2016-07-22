//like LV but flood at round 3
//not fast (as it is different from fast paxos)
//let's call it short

package example

import psync._
import psync.Time._
import psync.macros.Macros._

class SlvProcess extends Process[ConsensusIO] {

  var x = 0
  var ts = new Time(-1)
  var commit = false
  var vote = 0
  var decision = -1 //TODO as ghost
  var decided = false
  var callback: ConsensusIO = null

  //rotating coordinator
  def coord(phi: Int): ProcessID = new ProcessID((phi % n).toShort)

  def init(io: ConsensusIO) = i{
    callback = io
    x = io.initialValue
    ts = -1
    decided = false 
    commit = false
  }

  val rounds = phase(
    new Round[(Int,Time)]{

      def send(): Map[ProcessID,(Int, Time)] = {
        Map( coord(r/4) -> (x, ts) )
      }

      override def expectedNbrMessages = if (id == coord(r/4)) n/2 + 1 else 0

      def update(mailbox: Map[ProcessID,(Int, Time)]) {
        if (id == coord(r/4) && mailbox.size > n/2) {
          // let θ be one of the largest θ from 〈ν, θ〉received
          // vote(p) := one ν such that 〈ν, θ〉 is received
          vote = mailbox.maxBy(_._2._2)._2._1
          commit = true
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
        }
      }

    },

    new Round[Int]{

      def send(): Map[ProcessID,Int] = {
        if ( ts == (r/4) ) {
          broadcast(x)
        } else {
          Map.empty
        }
      }

      override def expectedNbrMessages = n/2 + 1

      def update(mailbox: Map[ProcessID,Int]) {
        if (mailbox.size > n/2) {
          val v = mailbox.head._2
          if (!decided) {
            callback.decide(v)
            decision = v
            decided = true
          }
        }
        commit = false
        if ((decided: Boolean)) {
          //terminate()
          exitAtEndOfRound()
        }
      }

    }

  )

}

class ShortLastVoting extends Algorithm[ConsensusIO,SlvProcess] {

  val spec = TrivialSpec
  
  def process = new SlvProcess

  def dummyIO = new ConsensusIO{
    val initialValue = 0
    def decide(value: Int) { }
  }
}
