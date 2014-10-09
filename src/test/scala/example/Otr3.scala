package example

import round._
import round.macros.Macros._

//like OTR but uses a boolean flag instead of an option for the decision
class OTR3 extends Algorithm[ConsensusIO] {


  import VarHelper._
  import SpecHelper._

  val V = new Domain[Int]

  //variables
  val x = new LocalVariable[Int](0)
  val decision = new LocalVariable[Int](-1) //TODO as ghost
  val decided = new LocalVariable[Boolean](false)


  val spec = new Spec {
      val safetyPredicate = f(true)
      val livnessPredicate = List( f( S.exists( s => P.forall( p => HO(p) == s && s.size > 2*n/3 ))))
      val invariants = List(
        f(  P.forall( i => !decided(i) )
         || V.exists( v => {
           val A = P.filter( i => x(i) == v);
           A.size > 2*n/3 && P.forall( i => decided(i) ==> (decision(i) == v))
        })),
        f(V.exists( v => {
           val A = P.filter( i => x(i) == v);
           A.size == n && P.forall( i => decided(i) ==> (decision(i) == v))
        })),
        f(V.exists( v => P.forall( i => decided(i) && decision(i) == v) ))
      ) //how to relate the invariants and the magic rounds

      val properties = List(
        ("Termination",    f(P.forall( i => decided(i)) )),
        ("Agreement",      f(P.forall( i => P.forall( j => (decided(i) && decided(j)) ==> (decision(i) == decision(j)) )))),
        ("Validity",       f(V.exists( v => P.forall( i => init(x)(i) == v ==> P.forall( j => decided(j) ==> (decision(j) == v) ))))),
        ("Integrity",      f(P.exists( j => P.forall( i => decided(i) ==> (decision(i) == init(x)(j)) )))),
        ("Irrevocability", f(P.forall( i => old(decided)(i) ==> (decided(i) && old(decision)(i) == decision(i)) )))
      )
  }
  
  
  def process(id: ProcessID, io: ConsensusIO) = p(new Process(id) {
      
    x <~ io.initialValue

    val rounds = Array[Round](
      rnd(new Round{

        type A = Int

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        //min most often received
        def mmor(mailbox: Set[(Int, ProcessID)]): Int = {
          val byValue = mailbox.groupBy(_._1)
          import scala.math.Ordered._
          val m = byValue.minBy{ case (v, procs) => (-procs.size.toLong, v) }
          //val m = byValue.minBy{ case (v, procs) => (-procs.size.toLong << 32) + v }
          m._1
        } ensuring { v1 =>
          mailbox.map(_._1).forall( v2 =>
            mailbox.filter(_._1 == v1).size > mailbox.filter(_._1 == v2).size || v1 <= v2
          )
        }

        def send(): Set[(Int, ProcessID)] = {
          broadcast(x) //macro for (x, All)
        }

        def update(mailbox: Set[(Int, ProcessID)]) {
          if (mailbox.size > 2*n/3) {
            val v = mmor(mailbox)
            x <~ v
            if (mailbox.filter(msg => msg._1 == v).size > 2*n/3) {
              if (!decided) {
                io.decide(v)
              }
              decided <~ true
              decision <~ v
            }
          }
        }

      })
    )

  })

}
