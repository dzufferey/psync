package example

import round._

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
      val safetyPredicate = true;
      val livnessPredicate = List( S.exists( s => P.forall( p => HO(p) == s && s.size > 2*n/3 )))
      val invariants = List(  P.forall( i => !decision(i).isEmpty )
                            || V.exists( v => {
                                  val A = P.filter( i => x(i) == v);
                                  A.size > 2*n/3 && P.forall( i => decision(i).isDefined ==> (decision(i).get == v))
                               }),
                             V.exists( v => {
                               val A = P.filter( i => x(i) == v);
                               A.size == n.get && P.forall( i => decision(i).isDefined ==> (decision(i).get == v))
                             }),
                             V.exists( v => P.forall( i => decision(i).isDefined ==> (decision(i).get == v)) )
                           ) //how to relate the invariants and the magic rounds

      val properties = List(
        ("Termination", P.forall( i => decision(i).isDefined) ),
        ("Agreement",   P.forall( i => P.forall( j => decision(i).isDefined && decision(j).isDefined ==> (decision(i).get == decision(j).get) ))),
        ("Validity",    V.exists( v => P.forall( i => init(x)(i) == v ==> P.forall( j => decision(j).isDefined ==> (decision(j).get == v) )))),
        ("Integrity",   P.exists( j => P.forall( i => decision(i).isDefined ==> (decision(i).get == init(x)(j)) ))),
        ("Irrevocability", P.forall( i => old(decision)(i).isDefined ==> (old(decision)(i) == decision(i)) ))
      )
  }
  
  
  def process(id: Short, io: OtrIO) = new Process(id) {
      
    x <~ io.initialValue

    def mmor(mailbox: Set[(Int, Process)]): Int = {
      sys.error("not yet implemented")
      0
    } ensuring { v1 => mailbox.map(_._1).forall(v2 => mailbox.filter(_._1 == v1).size > mailbox.filter(_._1 == v2).size || v1 <= v2) }

    val rounds = List(
      new Round[Int]{

        def send(): Set[(Int, Process)] = {
          broadcast(x) //macro for (x, All)
        }

        def update(mailbox: Set[(Int, Process)]) {
          if (mailbox.size > 2*n/3) {
            val v = mmor(mailbox)
            x <~ v
            if (mailbox.filter(msg => msg._1 == v).size > 2*n/3) {
              decision <~ Some(v);
              io.decide(v)
            }
          }
        }
      }
    )
  }

}
