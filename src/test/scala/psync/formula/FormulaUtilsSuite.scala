package psync.formula

import org.scalatest.funsuite._
import Common._
import FormulaUtils._

class FormulaUtilsSuite extends AnyFunSuite {

  val a = Variable("a").setType(Bool)
  val x = Variable("x").setType(Int)
  val p1 = Variable("p1").setType(pid)
  
  test("collecting types") {
    assert(collectTypes(And(And(Eq(a,a),Eq(x,x)), Eq(p1,p1))) == Set[Type](Bool, Int, pid))
    assert(collectTypes(And(Eq(a,a), Eq(p1,p1))) == Set[Type](Bool, pid))
    assert(collectTypes(And(Eq(x,x), Eq(p1,p1))) == Set[Type](Bool, Int, pid))
    assert(collectTypes(Eq(x,x)) == Set[Type](Bool, Int))
    assert(collectTypes(x) == Set[Type](Int))
  }

  test("ground terms 1") {
    val rqp1 = Application(r, List(Application(q, List(p1))))

    val ax = ForAll(List(p1), Eq(qp1,p2))
    val gts = Set[Formula](p2)

    val form1 = And(ax, Eq(rqp1, rp2))
    val gts1 = gts + rqp1 + qp1 + p1 + rp2 + p2
    assert(collectGroundTerms(form1) == gts1)

    val form2 = And(ax, Lt(rp2,rqp3))
    val gts2 = gts + rp2 + rqp3 + qp3 + p3 + Lt(rp2,rqp3)
    assert(collectGroundTerms(form2) == gts2)
  }
  
  test("ground terms 2") {
    val c1 = Comprehension(List(x), Leq(x, y) )
    val gts = Set[Formula](y,c1)
    assert(collectGroundTerms(c1) == gts)
    val f1 = ForAll(List(y), c1)
    assert(collectGroundTerms(f1).isEmpty)
    val f2 = ForAll(List(x), c1)
    assert(collectGroundTerms(f2) == gts)
  }

}
