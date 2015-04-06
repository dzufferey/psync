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

}
