package round.logic

import round.formula._
import round.formula.Common._

import org.scalatest._

class IncrementalGeneratorSuite extends FunSuite {

  test("formula 1"){
    val axs = List(
      ForAll(List(p1), Eq(rp1, IntLit(0))),
      ForAll(List(p2), Eq(rp2, IntLit(0))),
      ForAll(List(p1,p2), Eq(pp1, pp2))
    )
    val itg = new IncrementalFormulaGenerator(axs)
    val g1 = itg.generate(p1)
    assert(g1.size == 2)
    assert(g1 contains Eq(IntLit(0), rp1))
    assert(g1 contains Eq(pp1, pp1))
    val g2 = itg.generate(p2)
    assert(g2.size == 3)
    assert(g2 contains Eq(IntLit(0), rp2))
    assert(g2.contains(Eq(pp2, pp1)) || g2.contains(Eq(pp1, pp2)))
    assert(g2 contains Eq(pp2, pp2))
    val g3 = itg.generate(p2)
    assert(g3.isEmpty)
  }

}
