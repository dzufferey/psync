package psync.logic.quantifiers

import psync.formula._
import psync.formula.Common._
import psync.logic._

import org.scalatest._

class IncrementalGeneratorSuite extends FunSuite {

  val f1 = List(
    ForAll(List(p1), Eq(rp1, IntLit(0))),
    ForAll(List(p2), Eq(rp2, IntLit(0))),
    ForAll(List(p1,p2), Eq(pp1, pp2))
  )

  val f2 = List(
    ForAll(List(p1), Eq(qp1, p1))
  )

  val f3 = List(
    ForAll(List(x), Leq(x, Plus(x,IntLit(1))))
  )

  val f4 = List(
    ForAll(List(p1), Neq(qp1, p1))
  )

  test("formula 1, generate"){
    val itg = new IncrementalGenerator(f1, new Eager, new CongruenceClosure)
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
    assert(g2 == g3)
  }

  test("formula 1: eager,0,false"){
    val cc = new CongruenceClosure
    cc.repr(IntLit(0))
    cc.repr(p1)
    cc.repr(p2)
    val itg = new IncrementalGenerator(f1, new Eager, cc)
    val res = itg.saturate(Some(0), false)
    assert(res.size == 5)
    //println(res)
  }

  test("formula 1: eager,1,false"){
    val cc = new CongruenceClosure
    cc.repr(IntLit(0))
    cc.repr(p1)
    cc.repr(p2)
    val itg = new IncrementalGenerator(f1, new Eager, cc)
    val res = itg.saturate(Some(1), false)
    assert(res.size == 5)
    //println(res)
  }

  test("formula 1: eager,2,false"){
    val cc = new CongruenceClosure
    cc.repr(IntLit(0))
    cc.repr(p1)
    cc.repr(p2)
    val itg = new IncrementalGenerator(f1, new Eager, cc)
    val res = itg.saturate(Some(2), false)
    assert(res.size == 5)
    //println(res)
  }

  test("formula 1: guided,1,false"){
    val cc = new CongruenceClosure
    cc.repr(IntLit(0))
    cc.repr(p1)
    cc.repr(p2)
    val itg = new IncrementalGenerator(f1, new Guided, cc)
    val res = itg.saturate(Some(1), false)
    assert(res.size == 0)
    //println(res)
  }

  test("formula 1: guided,2,false"){
    val cc = new CongruenceClosure
    cc.repr(IntLit(0))
    cc.repr(p1)
    cc.repr(p2)
    val itg = new IncrementalGenerator(f1, new Guided, cc)
    val res = itg.saturate(Some(2), false)
    assert(res.size == 0)
    //println(res)
  }

  test("formula 2: eager,1,false"){
    val cc = new CongruenceClosure
    cc.repr(p1)
    val itg = new IncrementalGenerator(f2, new Eager, cc)
    val res = itg.saturate(Some(1), false)
    assert(res.size == 1)
    //println(res)
  }
  
  test("formula 2: eager,2,false"){
    val cc = new CongruenceClosure
    cc.repr(p1)
    val itg = new IncrementalGenerator(f2, new Eager, cc)
    val res = itg.saturate(Some(2), false)
    assert(res.size == 1)
    //println(res)
  }

  test("formula 2: guided,1,false,a"){
    val cc = new CongruenceClosure
    cc.repr(p1)
    val itg = new IncrementalGenerator(f2, new Guided, cc)
    val res = itg.saturate(Some(1), false)
    assert(res.size == 0)
    //println(res)
  }

  test("formula 2: guided,1,false,b"){
    val cc = new CongruenceClosure
    cc.repr(p1)
    cc.repr(qp1)
    val itg = new IncrementalGenerator(f2, new Guided, cc)
    val res = itg.saturate(Some(1), false)
    assert(res.size == 1)
    //println(res)
  }

  test("formula 3: eager,0..9,false"){
    for (i <- 0 until 10) {
      val cc = new CongruenceClosure
      cc.repr(IntLit(1))
      val itg = new IncrementalGenerator(f3, new Eager, cc)
      val res = itg.saturate(Some(i), false)
      assert(res.size == i+1)
      //println(res)
    }
  }
  
  test("formula 4: guided,-1,false"){
    val cc = new CongruenceClosure
    cc.repr(p1)
    cc.repr(qp1)
    val itg = new IncrementalGenerator(f2, new Guided, cc)
    val res = itg.saturate(Some(-1), false)
    assert(res.size == 0)
    //println(res)
  }
  
  test("formula 4: guided,-1,true"){
    val cc = new CongruenceClosure
    cc.repr(p1)
    cc.repr(qp1)
    val itg = new IncrementalGenerator(f2, new Guided, cc)
    val res = itg.saturate(Some(-1), true)
    assert(res.size == 1)
    //println(res)
  }
  
  test("formula 4: guided,0,false"){
    val cc = new CongruenceClosure
    cc.repr(p1)
    cc.repr(qp1)
    val itg = new IncrementalGenerator(f2, new Guided, cc)
    val res = itg.saturate(Some(0), false)
    assert(res.size == 1)
    //println(res)
  }
  
  test("formula 4: guided,1,false"){
    val cc = new CongruenceClosure
    cc.repr(p1)
    cc.repr(qp1)
    val itg = new IncrementalGenerator(f2, new Guided, cc)
    val res = itg.saturate(Some(1), false)
    assert(res.size == 1)
    //println(res)
  }

}
