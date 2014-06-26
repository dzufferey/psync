package example

import round._
import round.Algorithm._
import round.formula._
import round.runtime.Group
import round.macros.Macros._
import io.netty.buffer.ByteBuf

abstract class OtrIO {
  val initialValue: Int
  def decide(value: Int): Unit
}

class OTR extends Algorithm[OtrIO] {

  import VarHelper._
  import SpecHelper._

  val V = new Domain[Int]

  //variables
  val x = new LocalVariable[Int](0)
  val decision = new LocalVariable[Option[Int]](None)


  val spec = new Spec {
      val safetyPredicate = f(true)
      val livnessPredicate = List( f( S.exists( s => P.forall( p => HO(p) == s && s.size > 2*n/3 ))))
      val invariants = List(f(  P.forall( i => !decision(i).isEmpty )
                            || V.exists( v => {
                                  val A = P.filter( i => x(i) == v);
                                  A.size > 2*n/3 && P.forall( i => decision(i).isDefined ==> (decision(i).get == v))
                               })),
                             f(V.exists( v => {
                               val A = P.filter( i => x(i) == v);
                               A.size == n.get && P.forall( i => decision(i).isDefined ==> (decision(i).get == v))
                             })),
                             f(V.exists( v => P.forall( i => decision(i).isDefined ==> (decision(i).get == v)) ))
                           ) //how to relate the invariants and the magic rounds

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

    type T = Int
    val rounds = Array[Round[Int]](
      new Round[Int]{

        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros)
        //min most often received
        def mmor(mailbox: Set[(Int, ProcessID)]): Int = {
          val byValue = mailbox.groupBy(_._1)
          val m = byValue.minBy{ case (v, procs) => (-procs.size.toLong << 32) + v }
          //a cleaner way of selectin the element is:
          //  import scala.math.Ordered._
          //  val m = byValue.minBy{ case (v, procs) => (-procs.size.toLong, v) }
          m._1
        } ensuring { v1 =>
          mailbox.map(_._1).forall(v2 =>
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
                io.decide(v)
              }
              decision <~ Some(v);
            }
          }
        }

      }
    )

  })

}
