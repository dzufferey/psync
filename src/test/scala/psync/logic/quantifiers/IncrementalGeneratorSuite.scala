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
    val itg = new IncrementalGenerator(f1, new Eager(None), new CongruenceClosure)
    val g1 = itg.generate(p1)
    assert(g1.size == 1)
    assert(g1 contains Eq(IntLit(0), rp1))
    val g2 = itg.generate(p2)
    assert(g2.size == 2)
    assert(g2 contains Eq(IntLit(0), rp2))
    assert(g2.contains(Eq(pp2, pp1)) || g2.contains(Eq(pp1, pp2)))
    val g3 = itg.generate(p2)
    assert(g2 == g3)
  }

  test("formula 1: eager,1,false"){
    val cc = new CongruenceClosure
    cc.repr(IntLit(0))
    cc.repr(p1)
    cc.repr(p2)
    val itg = new IncrementalGenerator(f1, new Eager(Some(1)), cc)
    val res = itg.saturate(false)
    assert(res.size == 3)
    //println(res)
  }

  test("formula 1: eager,2,false"){
    val cc = new CongruenceClosure
    cc.repr(IntLit(0))
    cc.repr(p1)
    cc.repr(p2)
    val itg = new IncrementalGenerator(f1, new Eager(Some(2)), cc)
    val res = itg.saturate(false)
    assert(res.size == 3)
    //println(res)
  }

  test("formula 2: eager,1,false"){
    val cc = new CongruenceClosure
    cc.repr(p1)
    val itg = new IncrementalGenerator(f2, new Eager(Some(1)), cc)
    val res = itg.saturate(false)
    assert(res.size == 1)
    //println(res)
  }
  
  test("formula 2: eager,2,false"){
    val cc = new CongruenceClosure
    cc.repr(p1)
    val itg = new IncrementalGenerator(f2, new Eager(Some(2)), cc)
    val res = itg.saturate(false)
    assert(res.size == 1)
    //println(res)
  }

  test("stratification 1"){
    def mkItg(f: Formula) = {
      new IncrementalGenerator(List(f), new Eager(None), new CongruenceClosure, TypeStratification)
    }
    val itg1 = mkItg(ForAll(List(p1), Eq(rp1, IntLit(0))))
    assert(itg1.leftOver.size == 1)
    val itg2 = mkItg(ForAll(List(p1,p2), Eq(qp1, qp2)))
    assert(itg2.leftOver.size == 0)
  }

  test("stratification 2"){
    val cc = new CongruenceClosure
    cc.repr(p1)
    cc.repr(p2)
    val itg = new IncrementalGenerator(List(ForAll(List(p1,p2), Neq(qp1, p2))), new Eager(Some(1)), cc, TypeStratification)
    assert(itg.leftOver.size == 0)
    val res = itg.saturate(true)
    assert(res.size == 2)
  }

}
