package psync.macros

import psync.formula._

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

  test("Set operations") {
    val s = Set(1,2,3)
    Macros.asFormula( s contains 2 ) match {
      case Contains(Variable("s"), IntLit(2)) => ()
      case other => sys.error("unexpected: " + other)
    }
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

  test("overloading") {
    val s = Set(1)
    val m = Map(1 -> 1)
    Macros.asFormula( s.size ) match {
      case Cardinality(Variable("s")) => ()
      case other => sys.error("unexpected: " + other)
    }
    Macros.asFormula( m.size ) match {
      case Size(Variable("m")) => ()
      case other => sys.error("unexpected: " + other)
    }
  }
  
  test("Map apply, contains") {
    val m = Map(1 -> 1)
    Macros.asFormula( m(1) == 1 ) match {
      case Eq(LookUp(Variable("m"), IntLit(1)), IntLit(1)) => ()
      case other => sys.error("unexpected: " + other)
    }
    Macros.asFormula( m contains 1 ) match {
      case IsDefinedAt(Variable("m"), IntLit(1)) => ()
      case other => sys.error("unexpected: " + other)
    }
  }

  test("Map ∀,∃") {
    val m = Map(1 -> 2)
    Macros.asFormula( m.forall{ case (k, v) => k == v } ) match {
      case ForAll(List(v1), Implies(In(v2, KeySet(Variable("m"))), Eq(v3, LookUp(Variable("m"), v4))))
        if v1 == v2 && v2 == v3 && v3 == v4 => ()
      case other => sys.error("unexpected: " + other)
    }
    Macros.asFormula( m.exists{ case (k, v) => k == v } ) match {
      case Exists(List(v1), And(In(v2, KeySet(Variable("m"))), Eq(v3, LookUp(Variable("m"), v4))))
        if v1 == v2 && v2 == v3 && v3 == v4 => ()
      case other => sys.error("unexpected: " + other)
    }
  }

  test("Map filter") {
    val m = Map(1 -> 2)
    val m2 = Map(1 -> 2)
    Macros.asFormula( m2 == m.filter{ case (k, v) => k == v } ) match {
      case And(Eq(Variable("m2"), v1),
               ForAll(List(f1), Eq(LookUp(Variable("m"), f2), LookUp(v3, f3))),
               Eq(KeySet(v2), Comprehension(List(c1), And(In(c2, KeySet(Variable("m"))), Eq(c3, LookUp(Variable("m"), c4)))))
           ) if v1 == v2 && v2 == v3 && 
                c1 == c2 && c2 == c3 && c3 == c4 &&
                f1 == f2 && f2 == f3 => ()
      case other => sys.error("unexpected: " + other)
    }
  }
  
  test("Map map") {
    val m = Map(1 -> 2)
    val m2 = Map(1 -> 2)
    Macros.asFormula( m2 == (m.map{ case (k, v) => k -> (v + 1) }: Map[Int, Int]) ) match {
      case And(Eq(Variable("m2"), v1),
               ForAll(List(f1), Eq(LookUp(v3, f2), Plus(LookUp(Variable("m"), f3), IntLit(1)))),
               Eq(KeySet(v2), KeySet(Variable("m")))
           ) if v1 == v2 && v2 == v3 && 
                f1 == f2 && f2 == f3 => ()
      case other => sys.error("unexpected: " + other)
    }
  }
  
  test("Map empty") {
    val m = Map(1 -> 2)
    Macros.asFormula( m == Map.empty[Int,Int] ) match {
      case And(Eq(Variable("m"), v1),
               Eq(KeySet(v2), Comprehension(List(_), False()))
           ) if v1 == v2 => ()
      case other => sys.error("unexpected: " + other)
    }
  }

  test("Map construction") {
    val m = Map(1 -> 1)
    Macros.asFormula( m == Map( 1 -> 2, 3 -> 4 ) ) match {
      case And(Eq(Variable("m"), v1),
               Eq(KeySet(v4), Comprehension(List(e1), Or(Eq(e2, IntLit(1)), Eq(e3, IntLit(3))))),
               Eq(LookUp(v3, IntLit(3)), IntLit(4)),
               Eq(LookUp(v2, IntLit(1)), IntLit(2))
           ) if v1 == v2 && v2 == v3 && v3 == v4 &&
                e1 == e2 && e2 == e3 => ()
      case other => sys.error("unexpected: " + other)
    }
  }

  test("Time") {
    Macros.asFormula( new psync.Time( 42 ).toInt ) match {
      case a1 @ Application(t1, List(a2 @ Application(t2, List(IntLit(42)))))
        if t1 == psync.logic.ReduceTime.toInt && t2 == psync.logic.ReduceTime.fromInt &&
           a1.tpe == Int && a2.tpe == psync.logic.CL.timeType => ()
      case other => sys.error("unexpected: " + other)
    }
  }

}
