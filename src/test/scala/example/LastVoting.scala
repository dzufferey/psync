package example

import round._
import round.macros.Macros._

class LastVoting(afterDecision: Int = 1) extends Algorithm[ConsensusIO] {

  import VarHelper._
  import SpecHelper._

  val V = new Domain[Int]

  //variables
  val x = new LocalVariable[Int](0)
  val ts = new LocalVariable[Int](-1)
  val ready = new LocalVariable[Boolean](false)
  val commit = new LocalVariable[Boolean](false)
  val vote = new LocalVariable[Int](0)
  val decision = new LocalVariable[Int](-1) //TODO as ghost
  val decided = new LocalVariable[Boolean](false)
  val after = new LocalVariable[Int](afterDecision)

  //FIXME once the macro issue is sorted out ...
  //rotating coordinator
  def coord(phi: Int): ProcessID = new ProcessID((phi % n).toShort)

  val spec = new Spec {
      val livnessPredicate = List( f(P.exists( p => P.forall( q => p == coord(r/4) && HO(p).size > n/2 ) )) )

      val noDecision = f( P.forall( i => !decided(i) && !ready(i)) )

      val majority = f(
        V.exists( v => V.exists( t => {
            val A = P.filter( i => ts(i) >= t )
            A.size > n/2 &&
            t <= r/4 &&
            P.forall( i => (A.contains(i) ==> (x(i) == v) ) &&
                           (decided(i) ==> (decision(i) == v) ) &&
                           (commit(i) ==> (vote(i) == v) ) &&
                           (ready(i) ==> (vote(i) == v) ) &&
                           ((ts(i) == r/4) ==> commit(coord(r/4)) ))
        }) )
      )

      val keepInit = f ( P.forall( i => P.exists( j1 => x(i) == init(x)(j1) )) )

      val safetyInv = round.formula.And(keepInit, round.formula.Or(noDecision, majority))

      val invariants = List(
        safetyInv,
        f(P.exists( j => P.forall( i => decided(i) && decision(i) == init(x)(j)) ))
      )
      
      override val roundInvariants = List(
        List(
          round.formula.True(),
          f(P.exists( i => commit(i) ))
        ),
        List(
          round.formula.True(),
          f(P.exists( i => commit(i) && P.forall( j => ts(j) == r/4 && x(j) == vote(i) )))
        ),
        List(
          round.formula.True(),
          f(P.exists( i => commit(i) && ready(i) && P.forall( j => ts(j) == r/4 && x(j) == vote(i) )))
        )
      )

      val properties = List(
        ("Termination",    f(P.forall( i => decided(i)) )),
        ("Agreement",      f(P.forall( i => P.forall( j => (decided(i) && decided(j)) ==> (decision(i) == decision(j)) )))),
        ("Validity",       f(P.forall( i => decided(i) ==> P.exists( j => init(x)(j) == decision(i) )))),
        ("Integrity",      f(P.exists( j => P.forall( i => decided(i) ==> (decision(i) == init(x)(j)) )))),
        ("Irrevocability", f(P.forall( i => old(decided)(i) ==> (decided(i) && old(decision)(i) == decision(i)) )))
      )
  }
  
  def process(id: ProcessID, io: ConsensusIO) = p(new Process(id) {
      
    x <~ io.initialValue
    ts <~ -1
    decided <~ false 
    ready <~ false
    commit <~ false

    val rounds = Array[Round](
      rnd(new Round{

        type A = (Int, Int)

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

        //place holder for ACK
        type A = Int

        def send(): Set[(Int, ProcessID)] = {
          if ( ts == (r/4) ) {
            Set( (x: Int) -> coord(r/4) )
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

        type A = Int

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
            if (!decided) {
              io.decide(v)
              decision <~ v
              decided <~ true
            }
          }
          ready <~ false
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
