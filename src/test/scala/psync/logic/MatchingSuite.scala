package psync.logic

import psync.formula._
import psync.formula.Common._

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
    val msg1 = Matching.find(cc, fvs1, t1)
    assert(msg1.size == 2) 
    assert(msg1.contains(Map(x -> x)))
    assert(msg1.contains(Map(x -> y)))
    val t2 = g(x)
    val fvs2 = t2.freeVariables
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
    val msg1 = Matching.find(cc, fvs1, t1)
    assert(msg1.size == 1) 
    assert(msg1.contains(Map(x -> x)))
    val t2 = g(f(x))
    val fvs2 = t2.freeVariables
    val msg2 = Matching.find(cc, fvs2, t2)
    assert(msg2.isEmpty) 
  }
  
  test("term generated by 1"){
    val t1 = Eq(f(x), f(y))
    assert(Matching.termsGeneratedBy(Set(x,y), t1) == Set(f(x),f(y)))
    assert(Matching.termsGeneratedBy(Set(x), t1) == Set(f(x)))
    assert(Matching.termsGeneratedBy(Set(y), t1) == Set(f(y)))
    assert(Matching.termsGeneratedBy(Set(), t1).isEmpty)
  }
  
  test("term generated by 2"){
    val c = Comprehension(List(x), Leq(IntLit(0), x))
    val t1 = Leq(Cardinality(c), IntLit(0))
    intercept[java.lang.AssertionError]{ Matching.termsGeneratedBy(Set(x), t1)
    }
  }

  test("term generated by 3"){
    val c = Comprehension(List(x), Leq(IntLit(0), x))
    val t1 = Leq(Cardinality(c), IntLit(0))
    assert(Matching.termsGeneratedBy(Set(y), t1).isEmpty)
  }

  test("term generated by 4"){
    val c = Comprehension(List(x), Leq(y, x))
    val t1 = Leq(Cardinality(c), IntLit(0))
    assert(Matching.termsGeneratedBy(Set(y), t1) == Set(Cardinality(c)))
  }

}
