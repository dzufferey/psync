package round.macros

import round.formula._

import org.scalatest._

class FormulaExtractorSuite extends FunSuite {

  test("Int Literals") {
    Macros.asFormula( 123 ) match {
      case IntLit( 123 ) => ()
      case other => sys.error("unexpected: " + other)
    }
    Macros.asFormula( 123l ) match {
      case IntLit( 123 ) => ()
      case other => sys.error("unexpected: " + other)
    }
    Macros.asFormula( 123: Short ) match {
      case IntLit( 123 ) => ()
      case other => sys.error("unexpected: " + other)
    }
    Macros.asFormula( 123: Byte ) match {
      case IntLit( 123 ) => ()
      case other => sys.error("unexpected: " + other)
    }
  }
  
  test("Int operations") {
    val v = 123
    Macros.asFormula( v + 123 > v - v ) match {
      case Gt(Plus(Variable("v"), IntLit(123)), Minus(Variable("v"), Variable("v"))) => ()
      case other => sys.error("unexpected: " + other)
    }
  }

  test("Set bindings") {
    val s = Set(1,2,3)
    Macros.asFormula( s.forall(_ > 2) ) match {
      case ForAll(List(v1), Implies(In(v2,Variable("s")), Gt(v3, IntLit(2)))) if v1 == v2 && v2 == v3 => ()
      case other => sys.error("unexpected: " + other)
    }
    Macros.asFormula( s.exists(_ <= 2) ) match {
      case Exists(List(v1), And(In(v2,Variable("s")), Leq(v3, IntLit(2)))) if v1 == v2 && v2 == v3 => ()
      case other => sys.error("unexpected: " + other)
    }
    Macros.asFormula( s.filter(_ <= 2) ) match {
      case Comprehension(List(v1), And(In(v2,Variable("s")), Leq(v3, IntLit(2)))) if v1 == v2 && v2 == v3 => ()
      case other => sys.error("unexpected: " + other)
    }
    Macros.asFormula( s.map( _ + 2) ) match {
      case Comprehension(List(v1), And(In(Application(w, List(v2)), Variable("s")), Eq(v3, Plus(Application(w2, List(v4)), IntLit(2)))))
        if v1 == v2 && v2 == v3 && v3 == v4 && w == w2 && w.toString.startsWith("witness") => ()
      case other => sys.error("unexpected: " + other)
    }
    Macros.asFormula( Set.empty[Int] ) match {
      case Comprehension(List(v1), False() ) => ()
      case other => sys.error("unexpected: " + other)
    }
    Macros.asFormula( Set(1,2) ) match {
      case Comprehension(List(v1), Or(Eq(v2, IntLit(1)), Eq(v3, IntLit(2))) ) if v1 == v2 && v2 == v3  && v1.tpe == Int => ()
      case other => sys.error("unexpected: " + other)
    }
  }

  //TODO maps extraction

}
