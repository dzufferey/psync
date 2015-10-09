package psync.logic.quantifiers

import psync.formula._
import psync.formula.InlineOps._
import psync.formula.Common._
import psync.logic._

import org.scalatest._

class GuidedQuantifierInstSuite extends FunSuite {

  test("GuidedQuantifierInst 1") {
    val axs = List(
      ForAll(List(p1), Implies( qp1 === p2, qqp1 === p3 ))
    )
    val cc = new CongruenceClosure
    val itg = new GuidedQuantifierInst(axs, cc)
    //nothing
    val g1 = itg.generate(p1)
    assert(g1.size == 0)
    val g2 = itg.generate(p2)
    assert(g2.size == 0)
    val g3 = itg.generate(p3)
    assert(g3.size == 0)
    //
    cc.repr(qp1)
    val g4 = itg.generate(p1)
    assert(g4.size == 1)
    val g5 = itg.generate(p1)
    assert(g5.size == 0)
    //
    cc.repr(qqp2)
    val g6 = itg.generate(qp2)
    assert(g6.size == 1)
    val g7 = itg.generate(p2)
    assert(g7.size == 1)
    val g8 = itg.generate(qqp2)
    assert(g8.size == 0)
  }

  test("GuidedGenerator 1") {
    val axs = And(
      ForAll(List(p1), Implies( qp1 === p2, qqp1 === p3 ))
    )
    val cc = new CongruenceClosure
    val itg = new GuidedGenerator(axs, cc)
    //
    val g1 = itg.saturate(None, false)
    assert(g1.size == 0)
    //
    cc.repr(qp1)
    val g2 = itg.saturate(Some(5), false)
    assert(g2.size == 5)
  }

}
