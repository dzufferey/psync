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

}
