package example

import psync._
import psync.macros.Macros._
import psync.utils.serialization._
import psync.runtime.Runtime

abstract class MlvIO{
  // Left(p) = acceptor, Right(v) = proposer+acceptor
  val initialValue: Either[ProcessID,Int]
  def decide(value: Option[Int]): Unit
}

class MlvProcess(timeout: Long) extends Process[MlvIO] {
  
  var x: Option[Int] = None
  var ready = false
  var coord: Option[ProcessID] = None
  var callback: MlvIO = null

  def init(io: MlvIO) = i{
    callback = io
    io.initialValue match {
      case Left(c) =>
        coord = Some(c)
        x = None
      case Right(v) =>
        x = Some(v)
    }
    ready = false
  }

  val rounds = phase(
    new Round[Int](timeout){

      def send(): Map[ProcessID,Int] = {
        if (x.isDefined) {
          broadcast(x.get)
        } else {
          Map.empty[ProcessID,Int]
        }
      }

      override def expectedNbrMessages = if (r.toInt == 0) 1 else n

      def pickCoord(mailbox: Map[ProcessID, Int]): (ProcessID, Int) = {
        if (coord.isDefined && mailbox.contains(coord.get)) {
          coord.get -> mailbox(coord.get)
        } else {
          mailbox.minBy(_._1.id)
        }
      }

      def update(mailbox: Map[ProcessID,Int]): Unit = {
        //select by given coord
        if (mailbox.size > 0) {
          val vp = pickCoord(mailbox)
          coord = Some(vp._1)
          val v = vp._2
          assert(x.getOrElse(v) == v, "x = " + x + ", v = " + v)
          x = Some(v)
        }
      }

    },

    new Round[Int](timeout){

      def send(): Map[ProcessID,Int] = {
        if ( x.isDefined && coord.isDefined ) {
          Map( coord.get -> x.get )
        } else {
          Map.empty[ProcessID,Int]
        }
      }

      override def expectedNbrMessages =
        if (Some(id) == coord) n/2 + 1
        else 0

      def update(mailbox: Map[ProcessID,Int]): Unit = {
        if (mailbox.size > n/2) {
          ready = true
        }
      }

    },

    new Round[Int](timeout){

      def send(): Map[ProcessID,Int] = {
        if (ready) {
          broadcast(x.get)
        } else {
          Map.empty[ProcessID,Int]
        }
      }

      override def expectedNbrMessages = 1 

      def update(mailbox: Map[ProcessID,Int]): Unit = {
        if (mailbox.size > 0) {
          assert(mailbox.size == 1)
          val v = mailbox.head._2
          callback.decide(Some(v))
          exitAtEndOfRound()
        } else if (r > 30) {
          callback.decide(None) //leader probably crashed, need to start an election
          exitAtEndOfRound()
        }
        ready = false
        coord = None
      }

    }

  )

}

class MultiLastVoting(rt: Runtime, timeout: Long) extends Algorithm[MlvIO,MlvProcess](rt) {

  val spec = TrivialSpec
  
  def process = new MlvProcess(timeout)

  def dummyIO = new MlvIO{
    val initialValue: Either[ProcessID,Int] = Right(0)
    def decide(value: Option[Int]): Unit = { }
  }
}
