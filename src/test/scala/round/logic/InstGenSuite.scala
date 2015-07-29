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
    assert(FormulaUtils.getConjuncts(i1b).size == 4) //one cstr is actually redundant
    
    val ax2 = ForAll(List(x), Eq(f(x), g(x)))
    val i2a = InstGen.saturateWith(ax2, gts, gts, cc, Some(0), true)
    val i2b = InstGen.saturateWith(ax2, gts, gts, cc, Some(0), false)
    assert(FormulaUtils.getConjuncts(i2a).size == 1)
    assert(FormulaUtils.getConjuncts(i2b).size == 5)
  }

  test("local 2"){
    val fs = Eq(p1, p1)
    val cc = CongruenceClosure(fs)
    val gts = cc.groundTerms
    val s1 = Variable("s1").setType(FSet(pid))
    val ax1 = And(
      ForAll(List(p2), In(p2, CL.HO(p2))),
      ForAll(List(s1), SubsetEq(s1, s1))
    )
    val i1a = InstGen.saturateWith(ax1, gts, gts, cc, Some(0), true)
    val i1b = InstGen.saturateWith(ax1, gts, gts, cc, Some(0), false)
    val i1c = InstGen.saturateWith(ax1, gts, gts, cc, Some(1), false)
    val i1d = InstGen.saturate(ax1, gts)
    assert(i1a == True())
    assert(FormulaUtils.getConjuncts(i1b).size == 1)
    assert(FormulaUtils.getConjuncts(i1c).size == 2)
    assert(FormulaUtils.getConjuncts(i1d).size == 2)
  }

  test("saturate1"){
    val f0 = And( ForAll(List(x), Leq(f(x), x)),
                  Leq(f(x), g(y)),
                  Leq(f(y), g(y)))
    val r0 = InstGen.saturate1(f0, Some(0))
    assert(FormulaUtils.getConjuncts(r0).size == 7)

    val f1 = And( ForAll(List(x), Eq(f(x), x)),
                  Leq(f(x), g(y)),
                  Leq(f(y), g(y)))
    val r1 = InstGen.saturate1(f1, Some(0))
    assert(FormulaUtils.getConjuncts(r1).size == 6)
  }

}
