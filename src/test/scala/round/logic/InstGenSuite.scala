package round.logic

import round.formula._
import round.formula.Common._

import org.scalatest._

class InstGenSuite extends FunSuite {

  test("local 1"){
    val fs = And( Leq(f(x), g(y)),
                  Leq(f(y), g(y)))
    val cc = CongruenceClosure(fs)
    val gts = cc.groundTerms

    val ax1 = ForAll(List(x), Eq(f(x), x))
    val i1a = InstGen.saturateWith(ax1, gts, gts, cc, Some(0), true)
    val i1b = InstGen.saturateWith(ax1, gts, gts, cc, Some(0), false)
    assert(FormulaUtils.getConjuncts(i1a).size == 2)
    assert(FormulaUtils.getConjuncts(i1b).size == 5)
    
    val ax2 = ForAll(List(x), Eq(f(x), g(x)))
    val i2a = InstGen.saturateWith(ax2, gts, gts, cc, Some(0), true)
    val i2b = InstGen.saturateWith(ax2, gts, gts, cc, Some(0), false)
    assert(FormulaUtils.getConjuncts(i2a).size == 1)
    assert(FormulaUtils.getConjuncts(i2b).size == 5)
  }

}
