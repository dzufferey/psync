package round.logic

import round.formula._
import round.utils.smtlib._

import org.scalatest._

class CLSuite extends FunSuite {
  
  val pid = CL.procType

  val i = Variable("i").setType(pid)
  val j = Variable("j").setType(pid)
  
  val n = CL.n
  val nOver2 = Divides(n, Literal(2))

  val a = Variable("A").setType(FSet(pid))
  val b = Variable("b").setType(FSet(pid))

  val _data = UnInterpretedFct("data",Some(pid ~> Int))
  def data(i: Formula) = Application(_data, List(i)).setType(Int)

  val m = UnInterpretedFct("M",Some(pid ~> FSet(pid))) //sender mailbox, dual of HO
  val ho = CL.HO

  def assertUnsat(conjuncts: List[Formula]) {
    val c0 = conjuncts.map(Simplify.simplify)
    val f0 = c0.reduce( And(_, _) )
    val f1 = CL.reduce(f0)
    val solver = Solver(UFLIA)
    assert(!solver.testB(f1), "unsat formula")
  }

  def assertSat(conjuncts: List[Formula]) {
    val c0 = conjuncts.map(Simplify.simplify)
    val f0 = c0.reduce( And(_, _) )
    val f1 = CL.reduce(f0)
    val solver = Solver(UFLIA)
    assert( solver.testB(f1), "sat formula")
  }

  test("universe cardinality ⇒ ∀") {
    val fs = List(
      Eq(a, Comprehension(List(i), Eq(data(i), Literal(1)))),
      Eq(Cardinality(a), n),
      ForAll(List(i), Eq(data(i), Literal(0)))
    )
    assertUnsat(fs)
  }
  
  test("strict majorities intersect") {
    val fs = List(
      Eq(a, Comprehension(List(i), Eq(data(i), Literal(0)))),
      Eq(b, Comprehension(List(i), Eq(data(i), Literal(1)))),
      Gt(Cardinality(a), nOver2),
      Gt(Cardinality(b), nOver2)
    )
    assertUnsat(fs)
  }
  
  test("n = 0") {
    val fs = List(
      Eq(n, Literal(0))
    )
    assertUnsat(fs)
  }

  test("options") {
    val fs = List(
      IsDefined(FNone.application(Nil).setType(FOption(Int)))
    )
    assertUnsat(fs)
  }

}
