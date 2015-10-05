package psync.formula

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
    
  val a = Variable("a").setType(Int)
  val b = Variable("b").setType(Int)
  val c = Variable("c").setType(Int)
  val d = Variable("d").setType(Int)

  test("reverse pnf 1"){
    val f0 = Eq(a,b)
    val f1 = Eq(a,Plus(Literal(1),c))
    val f2 = Eq(a,c)
    val tst = ForAll(List(a,b), Exists(List(c), And(f0, Or(f1,f2))))
    val tst2 = Simplify.reversePnf(tst)
    val expected = And(ForAll(List(a,b), f0), ForAll(List(a), Or(Exists(List(c), f1), Exists(List(c), f2))))
    assert(Simplify.simplify(tst2) == Simplify.simplify(expected))
  }

  test("simplification 1") {
    val v = Variable("v").setType(Int)
    val s = Eq(v, Literal(1))
    assert( Simplify.simplifyBool(s) == Simplify.simplifyBool(And(s,s)), "similar elements should cancel")
  }

  test("simplification 2") {
    val v = Variable("v").setType(Int)
    val s = Eq(v, v)
    assert( True() == Simplify.simplify(And(s,s)))
  }

  test("de Bruijn 1") {
    val f = Exists(List(a), And(Exists(List(b), Eq(a, b)), Exists(List(c), Eq(a, c))))
    val f2 = Simplify.deBruijnIndex(f)
    val f3 = Simplify.simplify(f2)
    val i1 = Variable("Int_1").setType(Int)
    val i2 = Variable("Int_2").setType(Int)
    assert(f3 == Exists(List(i1, i2), Eq(i1, i2))) //due to normalization the indices are 1,2 instead of 2,1 but this is OK since they are bound at the same level.
  }
  
  test("split ∀ 1") {
    val f = ForAll(List(a,b,c), And(Eq(a, b), Eq(a, c)))
    val expected = And(ForAll(List(a,b), Eq(a,b)), ForAll(List(a,c), Eq(a,c)))
    val f2 = Simplify.splitForall(f)
    assert(Simplify.simplify(f2) == Simplify.simplify(expected))
    val f3 = Simplify.splitTopLevelForall(f)
    assert(Simplify.simplify(f3) == Simplify.simplify(expected))
  }
  
  test("split ∀ 2") {
    val f = Or(
        ForAll(List(a,b,c), And(Eq(a, b), Eq(a, c))),
        ForAll(List(a,b,c), And(Leq(a, b), Leq(a, c)))
      )
    val expected = Or(
        And(ForAll(List(a,b), Eq(a,b)), ForAll(List(a,c), Eq(a,c))),
        And(ForAll(List(a,b), Leq(a,b)), ForAll(List(a,c), Leq(a,c)))
      )
    val f2 = Simplify.splitForall(f)
    assert(Simplify.simplify(f2) == Simplify.simplify(expected))
    val f3 = Simplify.splitTopLevelForall(f)
    assert(Simplify.simplify(f3) == Simplify.simplify(f))
  }


  test("merge ∃ 1") {
    val f = Or(Exists(List(a,b), Eq(a,b)), Exists(List(a,c), Eq(a,c)))
    val expected = Exists(List(a,b), Eq(a,b))
    val f2 = Simplify.mergeExists(f)
    assert(Simplify.simplify(f2) == Simplify.simplify(expected))
  }

  test("merge ∃ 2") {
    val f = Or(Eq(d,d), Exists(List(a,b), Eq(a,b)), Exists(List(a,c), Eq(a,c)))
    val expected = Exists(List(a,b), Or(Eq(a,b), Eq(d,d)))
    val f2 = Simplify.mergeExists(f)
    assert(Simplify.simplify(f2) == Simplify.simplify(expected))
  }

}
