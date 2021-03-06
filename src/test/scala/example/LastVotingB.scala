package example

import psync._
import psync.Time._
import psync.macros.Macros._
import psync.utils.serialization._
import psync.runtime.Runtime
import SyncCondition._

abstract class BConsensusIO extends ConsensusIO[Array[Byte]] {
  val phase: Int
}

class LVBProcess(wholeCohort: SyncCondition, timeout: Long) extends Process[BConsensusIO] {
  
  //variables
  var phase = 0
  var x = Array[Byte]()
  var ts = new Time(-1)
  var ready = false
  var commit = false
  var vote: Array[Byte] = null
  var callback: BConsensusIO = null
  var expectedMajority = 0

  def init(io: BConsensusIO) = i{
    callback = io
    phase = io.phase.abs
    x = io.initialValue
    ts = -1
    ready = false
    commit = false
    expectedMajority = wholeCohort match {
      case Quorum => n/2 + 1
      case All => n
      case OnTO => n + 1
    }
  }

      
  def coord(phi: Int): ProcessID = new ProcessID(((phi + phase) % n).toShort)

  val rounds = psync.macros.Macros.phase(
    new Round[(Array[Byte], Time)](timeout){

      def send(): Map[ProcessID,(Array[Byte], Time)] = {
        if (r.toInt != 0 || id == coord(r/4))
          Map( coord(r/4) -> (x, ts) )
        else
          Map( )
      }

      override def expectedNbrMessages = {
        if (id == coord(r/4)) {
          if (r.toInt == 0) 1
          else expectedMajority
        } else 0
      }

      def update(mailbox: Map[ProcessID,(Array[Byte], Time)]): Unit = {
        if (id == coord(r/4) &&
            (mailbox.size > n/2 ||
             (r.toInt == 0 && mailbox.size > 0))) {
          val nemp = mailbox.filter(!_._2._1.isEmpty)
          if (nemp.isEmpty) {
            vote = Array[Byte]()
          } else {
            vote = nemp.maxBy(_._2._2)._2._1
          }
          commit = true
        }
      }

    },

    new EventRound[Array[Byte]]{
      
      def init = {
        if (id == coord(r/4) && !commit) Progress.goAhead
        else Progress.strictTimeout( timeout )
      }

      def send(): Map[ProcessID,Array[Byte]] = {
        if (id == coord(r/4) && commit) {
          broadcast(vote)
        } else {
          Map.empty[ProcessID,Array[Byte]]
        }
      }

      def receive(sender: ProcessID, payload: Array[Byte]) = {
        if (sender == coord(r/4)) {
          x = payload
          ts = r/4
          Progress.goAhead
        } else {
          Progress.unchanged
        }
      }

    },

    new Round[Unit](timeout){

      def send(): Map[ProcessID,Unit] = {
        if ( ts == (r/4) ) {
          Map( coord(r/4) -> () )
        } else {
          Map.empty[ProcessID,Unit]
        }
      }

      override def expectedNbrMessages = if (id == coord(r/4)) expectedMajority else 0

      def update(mailbox: Map[ProcessID,Unit]): Unit = {
        if (id == coord(r/4) && mailbox.size > n/2) {
          ready = true
        }
      }

    },

    //TODO we are sending againg the payload even though most processes already have it
    //this make sense if we similate the learning but otherwise it is rather expensive
    new Round[Unit](timeout){

      def send(): Map[ProcessID,Unit] = {
        if (id == coord(r/4) && ready) {
          broadcast( () )
        } else {
          Map.empty[ProcessID,Unit]
        }
      }

      override def expectedNbrMessages = 1 

      def update(mailbox: Map[ProcessID,Unit]): Unit = {
        if (mailbox contains coord(r/4)) {
          if ( ts == (r/4) ) {
            callback.decide(x)
          } else {
            //there is a decision but we did not get the data !!
            callback.decide(null)
          }
          exitAtEndOfRound()
        }
        ready = false
        commit = false
      }

    }
  )

}

class LastVotingB(rt: Runtime, timeout: Long, wholeCohort: SyncCondition) extends Algorithm[BConsensusIO,LVBProcess](rt) {

  val spec = TrivialSpec
  
  def process = new LVBProcess(wholeCohort, timeout)

  def dummyIO = new BConsensusIO{
    val phase = 0
    val initialValue = Array[Byte]()
    def decide(value: Array[Byte]): Unit = { }
  }
}
