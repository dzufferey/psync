package example

import psync._
import psync.Time._
import psync.macros.Macros._

abstract class BConsensusIO {
  val phase: Int
  val initialValue: Array[Byte]
  def decide(value: Array[Byte]): Unit
}

class LVBProcess(wholeCohort: Boolean) extends Process[BConsensusIO] {
  
  //variables
  var phase = 0
  var x = Array[Byte]()
  var ts = new Time(-1)
  var ready = false
  var commit = false
  var vote: Array[Byte] = null
  var callback: BConsensusIO = null

  def init(io: BConsensusIO) = i{
    callback = io
    phase = io.phase.abs
    x = io.initialValue
    ts = -1
    ready = false
    commit = false
  }

  def expectedMajority = if (wholeCohort) n else n/2 + 1
      
  def coord(phi: Int): ProcessID = new ProcessID(((phi + phase) % n).toShort)

  val rounds = psync.macros.Macros.phase(
    new Round[(Array[Byte], Time)]{

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

      def update(mailbox: Map[ProcessID,(Array[Byte], Time)]) {
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

    new Round[Array[Byte]]{

      def send(): Map[ProcessID,Array[Byte]] = {
        if (id == coord(r/4) && commit) {
          broadcast(vote)
        } else {
          Map.empty[ProcessID,Array[Byte]]
        }
      }

      override def expectedNbrMessages = 1

      def update(mailbox: Map[ProcessID,Array[Byte]]) {
        if (mailbox contains coord(r/4)) {
          x = mailbox(coord(r/4))
          ts = r/4
        }
      }

    },

    new Round[Int]{

      def send(): Map[ProcessID,Int] = {
        if ( ts == (r/4) ) {
          Map( coord(r/4) -> 0 )
        } else {
          Map.empty[ProcessID,Int]
        }
      }

      override def expectedNbrMessages = if (id == coord(r/4)) expectedMajority else 0

      def update(mailbox: Map[ProcessID,Int]) {
        if (id == coord(r/4) && mailbox.size > n/2) {
          ready = true
        }
      }

    },

    new Round[Array[Byte]]{

      def send(): Map[ProcessID,Array[Byte]] = {
        if (id == coord(r/4) && ready) {
          broadcast(vote)
        } else {
          Map.empty[ProcessID,Array[Byte]]
        }
      }

      override def expectedNbrMessages = 1 

      def update(mailbox: Map[ProcessID,Array[Byte]]) {
        if (mailbox contains coord(r/4)) {
          val v = mailbox(coord(r/4))
          callback.decide(v)
          exitAtEndOfRound()
        }
        ready = false
        commit = false
      }

    }
  )

}

class LastVotingB(wholeCohort: Boolean = false) extends Algorithm[BConsensusIO,LVBProcess] {

  val spec = TrivialSpec
  
  def process = new LVBProcess(wholeCohort)

  def dummyIO = new BConsensusIO{
    val phase = 0
    val initialValue = Array[Byte]()
    def decide(value: Array[Byte]) { }
  }
}
