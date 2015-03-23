package example

import round._
import round.macros.Macros._

abstract class BConsensusIO {
  val phase: Int
  val initialValue: Array[Byte]
  def decide(value: Array[Byte]): Unit
}

class LastVoting2B extends Algorithm[BConsensusIO] {

  import VarHelper._
  import SpecHelper._

  //variables
  val phase = new LocalVariable[Int](0)
  val x = new LocalVariable[Array[Byte]](Array())
  val ts = new LocalVariable[Int](-1)
  val ready = new LocalVariable[Boolean](false)
  val commit = new LocalVariable[Boolean](false)
  val vote = new LocalVariable[Array[Byte]](null)
  val callback = new LocalVariable[BConsensusIO](null)

  val spec = TrivialSpec
  
  def process = p(new Process[BConsensusIO]{
      
    def init(io: BConsensusIO) {
      callback <~ io
      phase <~ io.phase.abs
      x <~ io.initialValue
      ts <~ -1
      ready <~ false
      commit <~ false
    }

    val rounds = Array[Round](
      rnd(new Round{

        type A = (Array[Byte], Int)

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        def coord(phi: Int): ProcessID = new ProcessID(((phi + phase) % n).toShort)

        def send(): Set[((Array[Byte], Int), ProcessID)] = {
          Set((x: Array[Byte], ts: Int) -> coord(r / 4))
        }

        override def expectedNbrMessages = if (id == coord(r/4)) n/2 + 1 else 0

        def update(mailbox: Set[((Array[Byte], Int), ProcessID)]) {
          if (id == coord(r/4) && mailbox.size > n/2) {
            // let θ be one of the largest θ from 〈ν, θ〉received
            // vote(p) := one ν such that 〈ν, θ〉 is received
            val nemp = mailbox.filter(!_._1._1.isEmpty)
            if (nemp.isEmpty) {
              vote <~ Array[Byte]()
            } else {
              vote <~ nemp.maxBy(_._1._2)._1._1
            }
            commit <~ true
          }
        }

      }),

      rnd(new Round{

        type A = Array[Byte]

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        def coord(phi: Int): ProcessID = new ProcessID(((phi + phase) % n).toShort)

        def send(): Set[(Array[Byte], ProcessID)] = {
          if (id == coord(r/4) && commit) {
            broadcast(vote)
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages = 1

        def update(mailbox: Set[(Array[Byte], ProcessID)]) {
          val mb2 = mailbox.filter( _._2 == coord(r/4) )
          if (mb2.size > 0) {
            x <~ mb2.head._1
            ts <~ r/4
          }
        }

      }),

      rnd(new Round{

        //place holder for ACK
        type A = Int

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        def coord(phi: Int): ProcessID = new ProcessID(((phi + phase) % n).toShort)

        def send(): Set[(Int, ProcessID)] = {
          if ( ts == (r/4) ) {
            Set( 0 -> coord(r/4) )
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages = if (id == coord(r/4)) n/2 + 1 else 0

        def update(mailbox: Set[(Int, ProcessID)]) {
          if (id == coord(r/4) && mailbox.size > n/2) {
            ready <~ true
          }
        }

      }),

      rnd(new Round{

        type A = Array[Byte]

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        def coord(phi: Int): ProcessID = new ProcessID(((phi + phase) % n).toShort)

        def send(): Set[(Array[Byte], ProcessID)] = {
          if (id == coord(r/4) && ready) {
            broadcast(vote)
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages = 1 

        def update(mailbox: Set[(Array[Byte], ProcessID)]) {
          val mb2 = mailbox.filter( _._2 == coord(r/4) )
          if (mb2.size > 0) {
            val v = mb2.head._1
            assert(mb2.size == 1)
            callback.decide(v)
            exitAtEndOfRound()
          }
          ready <~ false
          commit <~ false
        }

      })

    )

  })

}
