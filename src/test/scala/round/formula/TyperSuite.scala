package round.formula

import org.scalatest._

import Typer._

class TyperSuite extends FunSuite {

  def tryType(f: Formula, t: Option[Type]) {
    Typer(f) match {
      case TypingSuccess(t1) =>
        t match {
          case Some(t2) =>
            if (t1.tpe != t2) {
              assert(false, "exptected " + t2 + ", found " + t1)
            }
          case None => assert(false, "exptected TypingFailure, found " + t1.tpe)
        }
      case TypingFailure(r) =>
        if (t.isDefined) {
          assert(false, "exptected " + t.get + " found TypingFailure("+r+")")
        }
      case TypingError(r) => assert(false, "Typer failed: " + r)
    }
  }

  test("should type (1)") { tryType(True(), Some(Bool)) }
  test("should type (2)") { tryType(False(), Some(Bool)) }
  test("should type (3)") { tryType(Literal(0), Some(Int)) }
  test("should type (4)") {
    val pid = UnInterpreted("ProcessID")
    val p = Variable("p").setType(pid)
    val x = UnInterpretedFct("x", Some(Function(List(pid), Int)))
    tryType(ForAll(List(p), Eq(Application(x, List(Variable("p"))), Literal(0))), Some(Bool))
  }
  test("should type (5)") {
    val pid = UnInterpreted("ProcessID")
    val p = Variable("p").setType(pid)
    val x = UnInterpretedFct("x")
    tryType(ForAll(List(p), Eq(Application(x, List(Variable("p"))), Literal(0))), Some(Bool))
  }
  
  test("should not type (1)") { tryType(Not(Literal(42)), None) }
  test("should not type (2)") {
    val pid = UnInterpreted("ProcessID")
    val p = Variable("p").setType(pid)
    val x = UnInterpretedFct("x")
    tryType(And(Eq(Application(x, List(Variable("p"))), Literal(0)), Eq(Application(x, List(Variable("p"))), True())), None)
  }


}
