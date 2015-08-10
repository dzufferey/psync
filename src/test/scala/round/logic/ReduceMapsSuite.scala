package round.logic

import round.formula._
import round.utils.smtlib._
import dzufferey.utils.Logger
import TestCommon._

import org.scalatest._

class ReduceMapsSuite extends FunSuite {

  //TODO How to specify maps by Comp
  
  val pid = CL.procType
  val n = CL.n
  
  val p1 = Variable("p1").setType(pid)
  val m = Variable("m").setType(FMap(pid, Int))

  test("isDefinedAt 1") {
    val fs = List(
      Eq(Size(m), IntLit(0)),
      IsDefinedAt(m, p1)
    )
    assertUnsat(fs)
  }

  test("isDefinedAt 2") {
    val fs = List(
      Eq(Size(m), IntLit(1)),
      IsDefinedAt(m, p1)
    )
    assertSat(fs)
  }

}
