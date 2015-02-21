package round.logic

import round.formula._
import round.utils.smtlib._

import org.scalatest._

class QuantifiersSuite extends FunSuite {

  test("isEPR 1") {
    import round.formula.Common._
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
    pos.foreach( f => assert( Quantifiers.isEPR(f)) )
    neg.foreach( f => assert(!Quantifiers.isEPR(f)) )
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
    assert(!Quantifiers.isEPR(f))
  }
  
  test("isStratified 1") {
    import round.formula.Common._
    def lt1(t1: Type, t2: Type): Boolean = (t1, t2) match {
      case (Int, `pid`) | (FSet(`pid`), `pid`) => true
      case _ => false
    }
    def lt2(t1: Type, t2: Type): Boolean = (t1, t2) match {
      case (Int, `pid`) | (`pid`, FSet(`pid`)) => true
      case _ => false
    }
    val f1 = ForAll(List(p1), Eq(rp1, Literal(0)))
    assert(Quantifiers.isStratified(f1, lt1))
    assert(Quantifiers.isStratified(f1, lt2))
    val f2 = ForAll(List(p1), Eq(rqp1, Literal(0)))
    assert(!Quantifiers.isStratified(f2, lt1))
    assert(!Quantifiers.isStratified(f2, lt2))
    val f3 = ForAll(List(p1), In(p1, CL.HO(p1)))
    assert( Quantifiers.isStratified(f3, lt1))
    assert(!Quantifiers.isStratified(f3, lt2))
  }

}

