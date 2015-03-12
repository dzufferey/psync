//like LV but flood at round 3
//not fast (as it is different from fast paxos)
//let's call it short

package example

import round._
import round.macros.Macros._

class ShortLastVoting(afterDecision: Int = 1) extends Algorithm[ConsensusIO] {

  import VarHelper._
  import SpecHelper._

  val V = new Domain[Int]

  //variables
  val x = new LocalVariable[Int](0)
  val ts = new LocalVariable[Int](-1)
  val commit = new LocalVariable[Boolean](false)
  val vote = new LocalVariable[Int](0)
  val decision = new LocalVariable[Int](-1) //TODO as ghost
  val decided = new LocalVariable[Boolean](false)
  val after = new LocalVariable[Int](afterDecision)
  //
  val callback = new LocalVariable[ConsensusIO](null)


  //FIXME once the macro issue is sorted out ...
  //rotating coordinator
  def coord(phi: Int): ProcessID = new ProcessID((phi % n).toShort)

  val spec = TrivialSpec
  
  def process = p(new Process[ConsensusIO]{
      
    def init(io: ConsensusIO) {
      callback <~ io
      x <~ io.initialValue
      ts <~ -1
      decided <~ false 
      commit <~ false
    }

    val rounds = Array[Round](
      rnd(new Round{

        type A = (Int, Int)

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        //rotating coordinator
        def coord(phi: Int): ProcessID = new ProcessID((phi % n).toShort)

        def send(): Set[((Int, Int), ProcessID)] = {
          Set((x: Int, ts: Int) -> coord(r / 4))
        }

        override def expectedNbrMessages = if (id == coord(r/4)) n/2 + 1 else 0

        def update(mailbox: Set[((Int, Int), ProcessID)]) {
          if (id == coord(r/4) && mailbox.size > n/2) {
            // let θ be one of the largest θ from 〈ν, θ〉received
            // vote(p) := one ν such that 〈ν, θ〉 is received
            vote <~ mailbox.maxBy(_._1._2)._1._1
            commit <~ true
          }
        }

      }),

      rnd(new Round{

        type A = Int

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        //rotating coordinator
        def coord(phi: Int): ProcessID = new ProcessID((phi % n).toShort)

        def send(): Set[(Int, ProcessID)] = {
          if (id == coord(r/4) && commit) {
            broadcast(vote)
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages = 1

        def update(mailbox: Set[(Int, ProcessID)]) {
          val mb2 = mailbox.filter( _._2 == coord(r/4) )
          if (mb2.size > 0) {
            x <~ mb2.head._1
            ts <~ r/4
          }
        }

      }),

      rnd(new Round{

        type A = Int

        def send(): Set[(Int, ProcessID)] = {
          if ( ts == (r/4) ) {
            broadcast(x)
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages = n/2 + 1

        def update(mailbox: Set[(Int, ProcessID)]) {
          if (mailbox.size > n/2) {
            val v = mailbox.head._1
            if (!decided) {
              callback.decide(v)
              decision <~ v
              decided <~ true
            }
          }
          commit <~ false
          if ((decided: Boolean)) {
            after <~ after - 1
            if(after <= 0) {
              terminate()
            }
          }
        }

      })

    )

  })

}
