package round.logic

import round.formula._
import round.formula.Common._

import org.scalatest._

class TermGeneratorSuite extends FunSuite {

  test("incremental 1"){
    val axs = List(
      ForAll(List(p1), Eq(rp1, IntLit(0))),
      ForAll(List(p2), Eq(rp2, IntLit(0))),
      ForAll(List(p1,p2), Eq(pp1, pp2))
    )
    val itg = new IncrementalTermGenerator(axs)
    val g1 = itg.generate(p1)
    assert(g1.size == 1)
    assert(g1 contains Eq(IntLit(0), rp1))
    val g2 = itg.generate(p2)
    assert(g2.size == 2)
    assert(g2 contains Eq(IntLit(0), rp2))
    assert(g2 contains Eq(pp1, pp2))
    val g3 = itg.generate(p2)
    assert(g3.isEmpty)
  }

}
