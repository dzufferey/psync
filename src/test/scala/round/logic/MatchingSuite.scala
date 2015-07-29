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
    val t1 = f(x)
    val fvs1 = t1.freeVariables
    //val ms1 = Matching(cc, fvs1, t1)
    //assert(ms1.size == 2) 
    //assert(ms1.contains(Map(x -> cc(x))))
    //assert(ms1.contains(Map(x -> cc(y))))
    val msg1 = Matching.find(cc, fvs1, t1)
    assert(msg1.size == 2) 
    assert(msg1.contains(Map(x -> x)))
    assert(msg1.contains(Map(x -> y)))
    val t2 = g(x)
    val fvs2 = t2.freeVariables
    //val ms2 = Matching(cc, fvs2, t2)
    //assert(ms2.size == 1) 
    //assert(ms2.contains(Map(x -> cc(y))))
    val msg2 = Matching.find(cc, fvs2, t2)
    assert(msg2.size == 1) 
    assert(msg2.contains(Map(x -> y)))
  }

  test("matching 2"){
    val fs = And(
      Eq(f(x), y),
      Eq(g(y), g(y))
    )
    val cc = CongruenceClosure(fs)
    val t1 = f(x)
    val fvs1 = t1.freeVariables
    //val ms1 = Matching(cc, fvs1, t1)
    //assert(ms1.size == 1) 
    //assert(ms1.contains(Map(x -> cc(x))))
    val msg1 = Matching.find(cc, fvs1, t1)
    assert(msg1.size == 1) 
    assert(msg1.contains(Map(x -> x)))
    val t2 = g(f(x))
    val fvs2 = t2.freeVariables
    //val ms2 = Matching(cc, fvs2, t2)
    //assert(ms2.size == 1) 
    //assert(ms2.contains(Map(x -> cc(x))))
    val msg2 = Matching.find(cc, fvs2, t2)
    assert(msg2.isEmpty) 
  }

}
