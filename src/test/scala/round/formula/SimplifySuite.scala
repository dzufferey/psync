package round.formula

import org.scalatest._

class SimplifySuite extends FunSuite {

  test("pnf 1") {
    import Simplify._
    val pid = UnInterpreted("ProcessID")
    val p = Variable("p").setType(pid)
    val x = UnInterpretedFct("x")
    val f = ForAll(List(p), Eq(Application(x, List(Variable("p"))), Literal(0)))
    //println(f)
    assert(isPnf(f))
    val f2 = pnf(f)
    //println(f2)
    assert(f == f2)
    val f3 = And(f,f)
    //println(f3)
    assert(!isPnf(f3))
    val f4 = pnf(f3)
    //println(f4)
    assert(isPnf(f4))
  }

  test("simplification 1") {
    val v = Variable("v").setType(Int)
    val s = Eq(v, Literal(1))
    assert( s == Simplify.simplifyBool(And(s,s)), "similar elements should cancel")
  }

  test("simplification 2") {
    val v = Variable("v").setType(Int)
    val s = Eq(v, v)
    assert( True() == Simplify.simplify(And(s,s)))
  }

  test("de Bruijn 1") {
    val a = Variable("a").setType(Int)
    val b = Variable("b").setType(Int)
    val c = Variable("c").setType(Int)
    val f = Exists(List(a), And(Exists(List(b), Eq(a, b)), Exists(List(c), Eq(a, c))))
    val f2 = Simplify.deBruijnIndex(f)
    val f3 = Simplify.simplify(f2)
    val i1 = Variable("Int_1").setType(Int)
    val i2 = Variable("Int_2").setType(Int)
    assert(f3 == Exists(List(i2, i1), Eq(i2, i1)))
  }

}
