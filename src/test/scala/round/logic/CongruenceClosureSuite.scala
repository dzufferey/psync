package round.logic

import round.formula._
import round.formula.Common._
import round.utils.smtlib._

import org.scalatest._

class CongruenceClosureSuite extends FunSuite {

  test("cc 1") {
    val eqs = And(
      Eq(p1, p2),
      Eq(qp2, p2),
      Eq(rp2, x),
      Eq(rqp1, y)
    )
    val cc = CongruenceClosure(eqs)
    assert(cc.repr(x) == cc.repr(y))
    assert(cc.normalize(pp1) == cc.normalize(pp2))
  }

  test("cc 2") {
    val cc = new CongruenceClosure
    cc.addConstraints(Eq(p1, p2))
    assert(cc.repr(pp2) == cc.repr(pp1))
    assert(cc.repr(qp1) != cc.repr(qp3))
    assert(cc.normalize(pp1) == cc.normalize(pp2))
  }
  
  test("cc 3") {
    val cc = new CongruenceClosure
    cc.addConstraints(Eq(p1, p2))
    assert(cc.normalize(pp1) == cc.normalize(pp2))
  }

  test("cc 4") {
    val cc = new CongruenceClosure
    cc.addConstraints(Eq(f(x), x))
    assert(cc.cClass(x).size == 2)
    assert(cc.cClass(x).contains(x))
    assert(cc.cClass(x).contains(f(x)))
    assert(cc.cClass(f(x)) == cc.cClass(x))
    assert(cc.cClass(y).size == 1)
  }

}
