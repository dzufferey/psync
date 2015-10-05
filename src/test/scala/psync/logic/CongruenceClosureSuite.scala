package psync.logic

import psync.formula._
import psync.formula.Common._
import psync.utils.smtlib._

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
    //due to "unstability" of the repr of a node might change when a new node is pushed in
    //so for terms which are not already in cc, it is safer to fist push them in the cc and then check equality
    cc.repr(pp2)
    cc.repr(pp1)
    assert(cc.repr(pp1) == cc.repr(pp2))
    cc.repr(qp1)
    cc.repr(qp3)
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

  test("cc 5") {
    val cc0 = new CongruenceClosure
    cc0.addConstraints(Eq(f(x), x))
    val cc = cc0.copy
    assert(cc.cClass(x).size == 2)
    assert(cc.cClass(x).contains(x))
    assert(cc.cClass(x).contains(f(x)))
    assert(cc.cClass(f(x)) == cc.cClass(x))
    assert(cc.cClass(y).size == 1)
  }

}
