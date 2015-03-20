package example

import round._
import round.macros.Macros._

abstract class MlvIO{
  // Left(p) = acceptor, Right(v) = proposer+acceptor
  val initialValue: Either[ProcessID,Int]
  def decide(value: Option[Int]): Unit
}

class MultiLastVoting extends Algorithm[MlvIO] {

  import VarHelper._
  import SpecHelper._

  val V = new Domain[Int]

  //variables
  val x = new LocalVariable[Option[Int]](None)
  val ready = new LocalVariable[Boolean](false)
  val coord = new LocalVariable[Option[ProcessID]](None)
  //
  val callback = new LocalVariable[MlvIO](null)

  val spec = TrivialSpec
  
  def process = p(new Process[MlvIO]{
      
    def init(io: MlvIO) {
      callback <~ io
      if (io.initialValue.isLeft) {
        coord <~ Some(io.initialValue.left.get)
        x <~ None
      } else {
//      coord <~ Some(id)
        x <~ Some(io.initialValue.right.get)
      }
      ready <~ false
    }

    val rounds = Array[Round](
      rnd(new Round{

        type A = Int

        def send(): Set[(Int, ProcessID)] = {
          if (x.isDefined) {
            broadcast(x.get)
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages = if (r == 0) 1 else n

        def pickCoord(mailbox: Set[(Int, ProcessID)]): (Int, ProcessID) = {
          if (coord.isDefined && mailbox.exists(_._2 == coord.get)) {
            mailbox.find(_._2 == coord.get).get
          } else {
            mailbox.minBy(_._2.id)
          }
        }

        def update(mailbox: Set[(Int, ProcessID)]) {
          //select by given coord
          if (mailbox.size > 0) {
            val vp = pickCoord(mailbox)
            coord <~ Some(vp._2)
            val v = vp._1
            assert(x.getOrElse(v) == v, "x = " + (x: Option[Int]) + ", v = " + v)
            x <~ Some(v)
          }
        }

      }),

      rnd(new Round{

        //place holder for ACK
        type A = Int

        def send(): Set[(Int, ProcessID)] = {
          if ( x.isDefined && coord.isDefined ) {
            Set( x.get -> coord.get )
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages =
          if (Some(id) == (coord: Option[ProcessID])) n/2 + 1
          else 0

        def update(mailbox: Set[(Int, ProcessID)]) {
          if (mailbox.size > n/2) {
            ready <~ true
          }
        }

      }),

      rnd(new Round{

        type A = Int

        def send(): Set[(Int, ProcessID)] = {
          if (ready) {
            broadcast(x.get)
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages = 1 

        def update(mailbox: Set[(Int, ProcessID)]) {
          if (mailbox.size > 0) {
            assert(mailbox.size == 1)
            val v = mailbox.head._1
            callback.decide(Some(v))
            exitAtEndOfRound()
          } else if (r > 30) {
            callback.decide(None) //leader probably crashed, need to start an election
            exitAtEndOfRound()
          }
          ready <~ false
          coord <~ None
        }

      })

    )

  })

}
