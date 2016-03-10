package psync.verification

import psync.formula._
import org.scalatest._
import scala.reflect.runtime.{universe => ru}

class A1 {

  @requires(Gt(Variable("v"), Literal(0)))
  @ensures("res", Eq(Variable("res"), Literal(0)))
  def m1(v: Int): Int = {
    assert(v > 0)
    0 // linter:ignore InvariantReturn
  }

}

class AnnotSuite extends FunSuite {

  val reqTpe = ru.typeOf[requires]
  val ensTpe = ru.typeOf[ensures]

  test("pre/post 1") {
    val cls = ru.typeOf[A1]
    for (method <- cls.members) {
      val as = method.annotations.filter(a => a.tree.tpe == reqTpe || a.tree.tpe == ensTpe)
      if(!as.isEmpty) {
        for (a <- as) {
          val expr = Annotations.eval(a) // linter:ignore UndesirableTypeInference
          expr match {
            case r: requires =>
              assert(r.f == Gt(Variable("v"), Literal(0)))
            case e: ensures =>
              assert(e.result == "res")
              assert(e.f == Eq(Variable("res"), Literal(0)))
            case _ =>
              assert(false)
          }
          //println("  as expr: " + expr)
        }
      }
    }
  }

}
