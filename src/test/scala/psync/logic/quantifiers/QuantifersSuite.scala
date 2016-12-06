package psync.logic.quantifiers

import psync.logic._
import psync.formula._

import org.scalatest._

class QuantifiersSuite extends FunSuite {

  test("isEPR 1") {
    import psync.formula.Common._
    val pos = List(
      Exists(List(b), And(a, Or(Not(a), b))),
      ForAll(List(a), And(a, Or(Not(a), b))),
      ForAll(List(x), Or(Leq(x, Literal(0)), Lt(Literal(0), x))),
      ForAll(List(x), Leq(x, Literal(0)))
    )
    val neg = List(
      ForAll(List(p1), Eq(qp1,p2)),
      ForAll(List(p1), Eq(qp1,p2))
    )
    pos.foreach( f => assert( isEPR(f)) )
    neg.foreach( f => assert(!isEPR(f)) )
  }
  
  test("isEPR 2") {
    val pid = CL.procType
    val i = Variable("i").setType(pid)
    val j = Variable("j").setType(pid)
    val a = Variable("A").setType(FSet(pid))
    val b = Variable("B").setType(FSet(pid))
    val _data = UnInterpretedFct("data",Some(pid ~> Int))
    def data(i: Formula) = Application(_data, List(i)).setType(Int)
    val f = ForAll(List(i), Eq(data(i), Literal(0)))
    assert(!isEPR(f))
  }
  
  test("isStratified 1") {
    import psync.formula.Common._
    val lt1 = new TypeStratification {
      def apply(t1: Type, t2: Type): Boolean = (t2, t1) match {
        case (Int, `pid`) | (FSet(`pid`), `pid`) => true
        case _ => false
      }
    }
    val lt2 = new TypeStratification {
      def apply(t1: Type, t2: Type): Boolean = (t2, t1) match {
        case (Int, `pid`) | (`pid`, FSet(`pid`)) => true
        case _ => false
      }
    }
    val f1 = ForAll(List(p1), Eq(rp1, Literal(0)))
    assert(lt1.isStratified(f1))
    assert(lt2.isStratified(f1))
    val f2 = ForAll(List(p1), Eq(rqp1, Literal(0)))
    assert(!lt1.isStratified(f2))
    assert(!lt2.isStratified(f2))
    val f3 = ForAll(List(p1), In(p1, CL.HO(p1)))
    assert( lt1.isStratified(f3))
    assert(!lt2.isStratified(f3))
  }
  
  test("isStratified 2") {
    import psync.formula.Common._
    val f1 = ForAll(List(p1), Eq(rp1, Literal(0)))
    assert(TypeStratification.isStratified(f1))
    val f2 = ForAll(List(p1), Eq(rqp1, Literal(0)))
    assert(!TypeStratification.isStratified(f2))
    val f3 = ForAll(List(p1), In(p1, CL.HO(p1)))
    assert(!TypeStratification.isStratified(f3))
  }

  test("isStratified 3") {
    import psync.formula.Common._
    val f1 = ForAll(List(x), Exists(List(p1), Eq(x, f(p1)) ))
    assert(!TypeStratification.isStratified(f1))
    val f2 = ForAll(List(p1), Exists(List(x), Eq(x, f(p1)) ))
    assert(TypeStratification.isStratified(f2))
  }

}

