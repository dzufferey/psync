package example

import round._
import round.macros.Macros._

//a version of OTR that eventually terminates
class OTR2(afterDecision: Int = 2) extends Algorithm[ConsensusIO] {

  import VarHelper._
  import SpecHelper._

  val V = new Domain[Int]

  //variables
  val x = new LocalVariable[Int](0)
  val decision = new LocalVariable[Option[Int]](None) //TODO as ghost ?
  val after = new LocalVariable[Int](afterDecision)
  //
  val callback = new LocalVariable[ConsensusIO](null)


  val spec = new Spec {
      val livenessPredicate = List( f( S.exists( s => P.forall( p => HO(p) == s && s.size > 2*n/3 ))))
      val invariants = List(
        f(  P.forall( i => !decision(i).isEmpty )
         || V.exists( v => {
           val A = P.filter( i => x(i) == v);
           A.size > 2*n/3 && P.forall( i => decision(i).isDefined ==> (decision(i).get == v))
        })),
        f(V.exists( v => {
           val A = P.filter( i => x(i) == v);
           A.size == n && P.forall( i => decision(i).isDefined ==> (decision(i).get == v))
        })),
        f(V.exists( v => P.forall( i => decision(i).isDefined ==> (decision(i).get == v)) ))
      ) //how to relate the invariants and the magic rounds

      val properties = List(
        ("Termination",    f(P.forall( i => decision(i).isDefined) )),
        ("Agreement",      f(P.forall( i => P.forall( j => decision(i).isDefined && decision(j).isDefined ==> (decision(i).get == decision(j).get) )))),
        ("Validity",       f(P.forall( i => decision(i).isDefined ==> P.exists( j => init(x)(j) == decision(i).get )))),
        ("Integrity",      f(P.exists( j => P.forall( i => decision(i).isDefined ==> (decision(i).get == init(x)(j)) )))),
        ("Irrevocability", f(P.forall( i => old(decision)(i).isDefined ==> (old(decision)(i) == decision(i)) )))
      )
  }
  
  
  def process = p(new Process[ConsensusIO]{
      
    def init(io: ConsensusIO) {
      callback <~ io
      x <~ io.initialValue
      decision <~ None
      after <~ afterDecision
    }

    val rounds = phase(
      new Round{

        type A = Int

        //min most often received
        def mmor(mailbox: Set[(Int, ProcessID)]): Int = {
          val byValue = mailbox.groupBy(_._1)
          import scala.math.Ordered._
          val m = byValue.minBy{ case (v, procs) => (-procs.size.toLong, v) }
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
              if (decision.isEmpty) {
                callback.decide(v)
              }
              decision <~ Some(v);
            }
          }
          if (decision.isDefined) {
            after <~ after - 1
            if(after <= 0) {
              terminate()
            }
          }
        }

      }
    )

  })

}
