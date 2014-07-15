package example

import round._
import round.Algorithm._
import round.macros.Macros._

class LastVoting extends Algorithm[ConsensusIO] {

  import VarHelper._
  import SpecHelper._

  val V = new Domain[Int]

  //variables
  val x = new LocalVariable[Int](0)
  val ts = new LocalVariable[Int](0)
  val ready = new LocalVariable[Boolean](false)
  val commit = new LocalVariable[Boolean](false)
  val vote = new LocalVariable[Int](0)
  val decision = new LocalVariable[Option[Int]](None) //TODO as ghost

  //FIXME once the macro issue is sorted out ...
  //rotating coordinator
  def coord(p: ProcessID, phi: Int): ProcessID = (phi % n).toShort

  val spec = new Spec {
      val safetyPredicate = f(true)
      val livnessPredicate = List( f(P.exists( p => P.forall( q => p == coord(q, r/4) && HO(p).size > n/2 ) )) )

      val noDecision = f( P.forall( i => decision(i).isEmpty && !ready(i)) )

      val majority = f(
        V.exists( v => V.exists( t => {
            val A = P.filter( i => ts(i) >= t )
            A.size > n/2 &&
            t <= r/4 &&
            P.forall( i => decision(i).isDefined ==> (decision(i).get == v) ) &&
            P.forall( i => commit(i) ==> (vote(i) == v) ) &&
            P.forall( i => ready(i) ==> (vote(i) == v) ) &&
            P.forall( i => (ts(i) == r/4) ==> commit(coord(i, r/4)) )
        }) )
      )


      val invariants = List(
        round.formula.Or(noDecision, majority)
        //TODO new part: ∃ i. commit(i)
        //TODO new part: ∃ v. ∀ i. ts(i) = r/4 ∧ x(i) = v
        //TODO new part: ∃ p. ready(p)
      )

      val properties = List(
        ("Termination",    f(P.forall( i => decision(i).isDefined) )),
        ("Agreement",      f(P.forall( i => P.forall( j => decision(i).isDefined && decision(j).isDefined ==> (decision(i).get == decision(j).get) )))),
        ("Validity",       f(V.exists( v => P.forall( i => init(x)(i) == v ==> P.forall( j => decision(j).isDefined ==> (decision(j).get == v) ))))),
        ("Integrity",      f(P.exists( j => P.forall( i => decision(i).isDefined ==> (decision(i).get == init(x)(j)) )))),
        ("Irrevocability", f(P.forall( i => old(decision)(i).isDefined ==> (old(decision)(i) == decision(i)) )))
      )
  }
  
  def process(id: ProcessID, io: ConsensusIO) = p(new Process(id) {
      
    x <~ io.initialValue
    ts <~ 0

    type T = Int

    val rounds = Array[Round](
      rnd(new Round{

        type A = (Int, Int)

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        //rotating coordinator
        def coord(p: ProcessID, phi: Int): ProcessID = {
          //println("trying to get a majority around: " + (x: Int, ts: Int, vote: Int, commit: Boolean, ready: Boolean, p, phi%n))
          (phi % n).toShort
        }

        def send(): Set[((Int, Int), ProcessID)] = {
          Set((x: Int, ts: Int) -> coord(id, r / 4))
        }

        override def expectedNbrMessages = if (id == coord(id, r/4)) n/2 + 1 else 0

        def update(mailbox: Set[((Int, Int), ProcessID)]) {
          if (id == coord(id, r/4) && mailbox.size > n/2) {
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
        def coord(p: ProcessID, phi: Int): ProcessID = {
          //println("trying to get a majority around: " + (x: Int, ts: Int, vote: Int, commit: Boolean, ready: Boolean, p, phi%n))
          (phi % n).toShort
        }

        def send(): Set[(Int, ProcessID)] = {
          if (id == coord(id, r/4) && commit) {
            broadcast(vote)
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages = 1

        def update(mailbox: Set[(Int, ProcessID)]) {
          val mb2 = mailbox.filter( _._2 == coord(id, r/4) )
          if (mb2.size > 0) {
            x <~ mb2.head._1
            ts <~ r/4
          }
        }

      }),

      rnd(new Round{

        //place holder for ACK
        type A = Boolean 

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        //rotating coordinator
        def coord(p: ProcessID, phi: Int): ProcessID = {
          //println("trying to get a majority around: " + (x: Int, ts: Int, vote: Int, commit: Boolean, ready: Boolean, p, phi%n))
          (phi % n).toShort
        }

        def send(): Set[(Boolean, ProcessID)] = {
          if ( ts == (r/4) ) {
            Set( true -> coord(id, r/4) )
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages = if (id == coord(id, r/4)) n/2 + 1 else 0

        def update(mailbox: Set[(Boolean, ProcessID)]) {
          if (id == coord(id, r/4) && mailbox.size > n/2) {
            ready <~ true
          }
        }

      }),

      rnd(new Round{

        type A = Int

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        //rotating coordinator
        def coord(p: ProcessID, phi: Int): ProcessID = {
          //println("trying to get a majority around: " + (x: Int, ts: Int, vote: Int, commit: Boolean, ready: Boolean, p, phi%n))
          (phi % n).toShort
        }

        def send(): Set[(Int, ProcessID)] = {
          if (id == coord(id, r/4) && ready) {
            broadcast(vote)
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages = 1 

        def update(mailbox: Set[(Int, ProcessID)]) {
          val mb2 = mailbox.filter( _._2 == coord(id, r/4) )
          if (mb2.size > 0) {
            val v = mb2.head._1
            if (decision.isEmpty) {
              io.decide(v)
              decision <~ Some(v)
            }
          }
          ready <~ false
          commit <~ false
        }

      })

    )

  })

}
