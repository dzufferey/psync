package psync.logic

import psync.formula._
import psync.formula.Common._

import org.scalatest.funsuite._

class RewritingTests extends AnyFunSuite {

  test("rule: fixed typed") {
    val r = RewriteRule(
      Set(x,y),
      Fst(Tuple(x,y)),
      x
    )
    val f = Fst(Tuple(Literal(1), Literal(2)))
    assert(r(f) == Literal(1))
  }

  test("rule: parametric") {
    val x = Variable("x").setType(TypeVariable("A"))
    val y = Variable("y").setType(TypeVariable("B"))
    val r = RewriteRule(
      Set(x,y),
      Fst(Tuple(x,y)),
      x
    )
    val f = Fst(Tuple(Literal(1), Literal(2)))
    val f2 = r(f)
    assert(f2 == Literal(1))
    assert(f2.tpe == Int)
  }

  test("rule: no match") {
    val x = Variable("x").setType(TypeVariable("A"))
    val r = RewriteRule(
      Set(x),
      Fst(Tuple(x,x)),
      x
    )
    val f = Fst(Tuple(Literal(1), Literal(2)))
    val f2 = r(f)
    assert(f2 == f)
  }

  test("rule: ill-typed") {
    val x = Variable("x").setType(TypeVariable("A"))
    val y = Variable("y").setType(TypeVariable("A"))
    val r = RewriteRule(
      Set(x,y),
      Fst(Tuple(x,y)),
      x
    )
    val f = Fst(Tuple(Literal(1), p1))
    assert(try {
      val f2 = r(f)
      false
    } catch {
      case e: java.lang.AssertionError =>
        true
    })
  }
  
  test("rule harder match 1") {
    val x = Variable("x").setType(TypeVariable("A"))
    val y = Variable("y").setType(TypeVariable("B"))
    val r = RewriteRule(
      Set(x,y),
      Fst(Tuple(x,y)),
      x
    )
    val f = Fst(Tuple(Get(FSome(Literal(1))), p1))
    val f2 = r(f)
    assert(f2 == Get(FSome(Literal(1))))
  }
  
  test("rule harder match 2") {
    val x = Variable("x").setType(TypeVariable("A"))
    val r = RewriteRule(
      Set(x),
      Get(FSome(x)),
      x
    )
    val f = Fst(Tuple(Get(FSome(Literal(1))), p1))
    val f2 = r(f)
    assert(f2 == Fst(Tuple(Literal(1), p1)))
  }
  
  test("rewriting 1") {
    val f = Fst(Tuple(Literal(1), p1))
    val f2 = Rewriting(f)
    assert(f2 == Literal(1))
  }

  test("rewriting 2") {
    val f = Fst(Tuple(Get(FSome(Literal(1))), p1))
    val f2 = Rewriting(f)
    assert(f2 == Literal(1))
  }

}
