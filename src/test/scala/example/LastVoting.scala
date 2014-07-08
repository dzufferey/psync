package example

import round._
import round.Algorithm._
import round.macros.Macros._

abstract class LvIO {
  val initialValue: Int
  def decide(value: Int): Unit
}

class LastVoting extends Algorithm[OtrIO] {

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


  val spec = new Spec {
      val safetyPredicate = f(true)
      val livnessPredicate = List( f(true) )
   // ∃ p. ∀ q. p = Coord(q, φ) ∧ p ∈ HO(q) ∧ |HO(p)| > n/2
      val invariants = List( f(true)
//  ∀ i. ¬decided(i) ∧ ¬ready(i)
//∨ ∃ v, t.   A = { i | ts(i) > t }
//          ∧ |A| > n/2
//          ∧ ∀ i ∈ A. x(i) = v
//          ∧ ∀ i. decided(i) ⇒ x(i) = v
//          ∧ ∀ i. commit(i) ⇒ vote(i) = v
//          ∧ ∀ i. ready(i) ⇒ vote(i) = v
//          ∧ t ≤ r/4
//          ∧ ∀ i. ts(i) = r/4 ⇒ commit(Coord(i))

//Invariant (new part): ∃ i. commit(i)

//Invariant (new part): ∃ v. ∀ i. ts(i) = r/4 ∧ x(i) = v

//Invariant (new part): ∃ p. ready(p)

      ) 
      val properties = List(
        ("Termination",    f(P.forall( i => decision(i).isDefined) )),
        ("Agreement",      f(P.forall( i => P.forall( j => decision(i).isDefined && decision(j).isDefined ==> (decision(i).get == decision(j).get) )))),
        ("Validity",       f(V.exists( v => P.forall( i => init(x)(i) == v ==> P.forall( j => decision(j).isDefined ==> (decision(j).get == v) ))))),
        ("Integrity",      f(P.exists( j => P.forall( i => decision(i).isDefined ==> (decision(i).get == init(x)(j)) )))),
        ("Irrevocability", f(P.forall( i => old(decision)(i).isDefined ==> (old(decision)(i) == decision(i)) )))
      )
  }
  
  def process(id: ProcessID, io: OtrIO) = p(new Process(id) {
      
    x <~ io.initialValue
    ts <~ 0

    type T = Int

    val rounds = Array[Round](
      rnd(new Round{

        type A = (Int, Int)

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        //rotating coordinator
        def coord(phi: Int): ProcessID = (phi % n).toShort

        def send(): Set[((Int, Int), ProcessID)] = {
          Set((x: Int, ts: Int) -> coord(r / 4))
        }

        override def expectedNbrMessages = if (id == coord(r/4)) n/2 + 1 else 0

        def update(mailbox: Set[((Int, Int), ProcessID)]) {
          if (id == coord(r/4) && mailbox.size > n/2) {
            // let θ be one of the largest θ from 〈ν, θ〉received
            // vote(p) := one ν such that 〈ν, θ〉 is received
            vote <~ mailbox.maxBy(_._1._2)._1._1
          }
        }

      }),

      rnd(new Round{

        type A = Int

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        //rotating coordinator
        def coord(phi: Int): ProcessID = (phi % n).toShort

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

        type A = Unit

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        //rotating coordinator
        def coord(phi: Int): ProcessID = (phi % n).toShort

        def send(): Set[(Unit, ProcessID)] = {
          if ( ts == (r/4) ) {
            Set( () -> coord(r/4) )
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages = if (id == coord(r/4)) n/2 + 1 else 0

        def update(mailbox: Set[(Unit, ProcessID)]) {
          if (id == coord(r/4) && mailbox.size > n/2) {
            ready <~ true
          }
        }

      }),

      rnd(new Round{

        type A = Int

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        //rotating coordinator
        def coord(phi: Int): ProcessID = (phi % n).toShort

        def send(): Set[(Int, ProcessID)] = {
          if (id == coord(r/4) && ready) {
            broadcast(vote)
          } else {
            Set.empty
          }
        }

        override def expectedNbrMessages = 1 

        def update(mailbox: Set[(Int, ProcessID)]) {
          val mb2 = mailbox.filter( _._2 == coord(r/4) )
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
