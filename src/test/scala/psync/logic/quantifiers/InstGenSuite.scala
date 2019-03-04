package psync.logic.quantifiers

import psync.formula._
import psync.formula.Common._
import psync.logic._

import org.scalatest._

class InstGenSuite extends FunSuite {

  implicit val namer = new dzufferey.utils.Namer

  test("local 1"){
    val fs = And( Leq(f(x), g(y)),
                  Leq(f(y), g(y)))
    val cc = CongruenceClosure(fs)
    val gts = cc.groundTerms

    val ax1 = ForAll(List(x), Eq(f(x), x))
    val i1a = InstGen.saturateWith(ax1, gts, Some(0), cc)
    val i1b = InstGen.saturateWith(ax1, gts, Some(1), cc)
    assert(FormulaUtils.getConjuncts(i1a).size == 2)
    assert(FormulaUtils.getConjuncts(i1b).size == 5) //XXX has some redundant cstr
    
    val ax2 = ForAll(List(x), Eq(f(x), g(x)))
    val i2a = InstGen.saturateWith(ax2, gts, Some(0), cc)
    val i2b = InstGen.saturateWith(ax2, gts, Some(1), cc)
    assert(FormulaUtils.getConjuncts(i2a).size == 1)
    assert(FormulaUtils.getConjuncts(i2b).size == 5) //XXX has some redundant cstrs
  }

  test("local 2"){
    val fs = Eq(p1, p1)
    val cc = CongruenceClosure(fs)
    val gts = cc.groundTerms
    val s1 = Variable("s1").setType(FSet(pid))
    val s2 = Variable("s2").setType(FSet(pid))
    val ax1 = And(
      ForAll(List(p2), In(p2, CL.HO(p2))),
      ForAll(List(s1), SubsetEq(s1, s2))
    )
    val i1a = InstGen.saturateWith(ax1, gts, Some(0), cc)
    val i1b = InstGen.saturateWith(ax1, gts, Some(1), cc)
    val i1c = InstGen.saturate(ax1)
    val i1d = InstGen.saturate(ax1, None, cc)
    val i1e = InstGen.saturateWith(ax1, gts, None, cc)
    assert(i1a == True())
    assert(FormulaUtils.getConjuncts(i1b).size == 2)
    assert(FormulaUtils.getConjuncts(i1c).size == 0)
    //assert(FormulaUtils.getConjuncts(i1d).size == 2) //XXX weird ?!
    assert(FormulaUtils.getConjuncts(i1e).size == 2)
  }

  test("saturate 1"){
    val f0 = And( ForAll(List(x), Leq(f(x), x)),
                  Leq(f(x), g(y)),
                  Leq(f(y), g(y)))
    val r0a = InstGen.saturate(f0, Some(0))
    assert(FormulaUtils.getConjuncts(r0a).size == 4)
    val r0b = InstGen.saturate(f0, Some(1))
    assert(FormulaUtils.getConjuncts(r0b).size == 7)

    val f1 = And( ForAll(List(x), Eq(f(x), x)),
                  Leq(f(x), g(y)),
                  Leq(f(y), g(y)))
    val r1a = InstGen.saturate(f1, Some(0))
    assert(FormulaUtils.getConjuncts(r1a).size == 4)
    val r1b = InstGen.saturate(f1, Some(1))
    assert(FormulaUtils.getConjuncts(r1b).size == 7)
  }
  
}
