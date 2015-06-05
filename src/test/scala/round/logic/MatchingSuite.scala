package round.logic

import round.formula._
import round.formula.Common._

import org.scalatest._

class MatchingSuite extends FunSuite {

  test("matching 1"){
    val fs = And(
      Leq(f(x), g(y)),
      Leq(f(y), g(y))
    )
    val cc = CongruenceClosure(fs)
    val m = new Matching(cc)
    val t1 = f(x)
    val fvs1 = t1.freeVariables
    val ms1 = m(t1, fvs1)
    assert(ms1.size == 2) 
    assert(ms1.contains(Map(x -> cc(x))))
    assert(ms1.contains(Map(x -> cc(y))))
    val t2 = g(x)
    val fvs2 = t2.freeVariables
    val ms2 = m(t2, fvs2)
    assert(ms2.size == 1) 
    assert(ms2.contains(Map(x -> cc(y))))
  }

}
