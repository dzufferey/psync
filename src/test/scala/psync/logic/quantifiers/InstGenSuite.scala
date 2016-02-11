package psync.logic.quantifiers

import psync.formula._
import psync.formula.Common._
import psync.logic._

import org.scalatest._

class InstGenSuite extends FunSuite {

  test("local 1"){
    val fs = And( Leq(f(x), g(y)),
                  Leq(f(y), g(y)))
    val cc = CongruenceClosure(fs)
    val gts = cc.groundTerms

    val ax1 = ForAll(List(x), Eq(f(x), x))
    val i1a = InstGen.saturateWith(ax1, gts, Some(-1), cc)
    val i1b = InstGen.saturateWith(ax1, gts, Some(0), cc)
    assert(FormulaUtils.getConjuncts(i1a).size == 2)
    assert(FormulaUtils.getConjuncts(i1b).size == 5) //XXX has some redundant cstr
    
    val ax2 = ForAll(List(x), Eq(f(x), g(x)))
    val i2a = InstGen.saturateWith(ax2, gts, Some(-1), cc)
    val i2b = InstGen.saturateWith(ax2, gts, Some(0), cc)
    assert(FormulaUtils.getConjuncts(i2a).size == 1)
    assert(FormulaUtils.getConjuncts(i2b).size == 5) //XXX has some redundant cstrs
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
    val i1a = InstGen.saturateWith(ax1, gts, Some(-1), cc)
    val i1b = InstGen.saturateWith(ax1, gts, Some(0), cc)
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
    val r0a = InstGen.saturate(f0, Some(-1))
    assert(FormulaUtils.getConjuncts(r0a).size == 4)
    val r0b = InstGen.saturate(f0, Some(0))
    assert(FormulaUtils.getConjuncts(r0b).size == 7)

    val f1 = And( ForAll(List(x), Eq(f(x), x)),
                  Leq(f(x), g(y)),
                  Leq(f(y), g(y)))
    val r1a = InstGen.saturate(f1, Some(-1))
    assert(FormulaUtils.getConjuncts(r1a).size == 4)
    val r1b = InstGen.saturate(f1, Some(0))
    assert(FormulaUtils.getConjuncts(r1b).size == 7)
  }
  
  /* XXX simplification makes that test obsolete
  test("saturate 2"){
    val f0 = And( ForAll(List(x), Geq(Plus(x,Literal(1)), x)),
                  Leq(IntLit(1), IntLit(1)))
    (-1 until 9).foreach(i => {
      val sat = InstGen.saturate(f0, Some(i))
      assert(FormulaUtils.getConjuncts(sat).size == i + 2)
    })
  }
  */
  
  /* XXX simplification makes that test obsolete
  test("saturate 3"){
    val f = And(
      ForAll(List(p1), Exists(List(p2), ForAll(List(p3), Or(Not(Eq(p1,p2)), Eq(p1, p3))))),
      Eq(p1,rp1)
    )
    val r0 = InstGen.saturate(f, Some(-1))
    assert(FormulaUtils.getConjuncts(r0).size == 1)
    val r1 = InstGen.saturate(f, Some(0))
    assert(FormulaUtils.getConjuncts(r1).size == 3)
    val r2 = InstGen.saturate(f, Some(1))
    assert(FormulaUtils.getConjuncts(r2).size == 7)
  }
  */

}
