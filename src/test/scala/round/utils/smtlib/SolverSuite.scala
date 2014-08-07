package round.utils.smtlib

import round.formula._

import org.scalatest._

class SolverSuite extends FunSuite {

  //TODO
  //- model extraction

  //import dzufferey.utils.Logger
  //Logger.moreVerbose
  //Logger.moreVerbose

  val pid = UnInterpreted("ProcessID")

  val a = Variable("a").setType(Bool)
  val b = Variable("b").setType(Bool)

  val x = Variable("x").setType(Int)
  val y = Variable("y").setType(Int)
  
  val p1 = Variable("p1").setType(pid)
  val p2 = Variable("p2").setType(pid)
  val p3 = Variable("p3").setType(pid)

  val f = UnInterpretedFct("f", Some(Function(List(Int), Int)))
  val g = UnInterpretedFct("g", Some(Function(List(Int), Int)))
  
  val p = UnInterpretedFct("p", Some(Function(List(pid), Bool)))
  val q = UnInterpretedFct("q", Some(Function(List(pid), pid)))
  val r = UnInterpretedFct("r", Some(Function(List(pid), Int)))

  val pp1 = Application(p, List(p1))
  val pp2 = Application(p, List(p2))
  val qp1 = Application(q, List(p1))
  val qp2 = Application(q, List(p2))
  val qp3 = Application(q, List(p3))
  val rp1 = Application(r, List(p1))
  val rp2 = Application(r, List(p2))
  val qqp1 = Application(q, List(qp1))
  val qqp2 = Application(q, List(qp2))
  val pqp1 = Application(p, List(qp1))
  val pqp2 = Application(p, List(qp2))
  val rqp1 = Application(r, List(Application(q, List(p1))))
  val rqp3 = Application(r, List(Application(q, List(p3))))

  test("propositional formula") {
    val form1 = And(a, Or(Not(a), b))
    val form2 = And(a, Not(a))
    val solver = Solver(QF_UF)
    assert( solver.test(form1).get, "sat formula")
    assert(!solver.test(form2).get, "unsat formula")
  }

  test("qbf") {
    val form1 = Exists(List(b), And(a, Or(Not(a), b)))
    val form2 = ForAll(List(a), And(a, Or(Not(a), b)))
    val solver = Solver(UF)
    assert( solver.test(form1).get, "sat formula")
    assert(!solver.test(form2).get, "unsat formula")
  }

  test("qf_uf") {
    val form1 = And(pqp1,Not(pqp2))
    val form2 = And(Eq(p1,p2), And(pqp1,Not(pqp2)))
    val solver = Solver(QF_UF)
    assert( solver.test(form1).get, "sat formula")
    assert(!solver.test(form2).get, "unsat formula")
  }
  
  test("uf") {
    val ax = ForAll(List(p1), Eq(qp1,p2))
    val form1 = And(ax, pqp1)
    val form2 = And(ax, And(pqp1,Not(pp2)))
    val solver = Solver(UF)
    assert( solver.test(form1).get, "sat formula")
    assert(!solver.test(form2).get, "unsat formula")
  }

  test("qf_lia") {
    val form1 = And(Eq(x, y), Eq(Literal(0), Minus(x,y)))
    val form2 = And(Not(Eq(x, y)), Eq(Literal(0), Minus(x,y)))
    val solver = Solver(QF_LIA)
    assert( solver.test(form1).get, "sat formula")
    assert(!solver.test(form2).get, "unsat formula")
  }
  
  test("lia") {
    val form1 = ForAll(List(x), Or(Leq(x, Literal(0)), Lt(Literal(0), x)))
    val form2 = ForAll(List(x), Leq(x, Literal(0)))
    val solver = Solver(LIA)
    assert( solver.test(form1).get, "sat formula")
    assert(!solver.test(form2).get, "unsat formula")
  }

  test("qf_uflia") {
    val form1 = Eq(rqp1, rp2)
    val form2 = And(Eq(p2,qp3), Lt(rp2,rqp3))
    val solver = Solver(QF_UFLIA)
    assert( solver.test(form1).get, "sat formula")
    assert(!solver.test(form2).get, "unsat formula")
  }
  
  test("uflia") {
    val ax = ForAll(List(p1), Eq(qp1,p2))
    val form1 = And(ax, Eq(rqp1, rp2))
    val form2 = And(ax, Lt(rp2,rqp3))
    val solver = Solver(UFLIA)
    assert( solver.test(form1).get, "sat formula")
    assert(!solver.test(form2).get, "unsat formula")
  }

  test("overloading") {
    val t1 = UnInterpreted("T1")
    val t2 = UnInterpreted("T2")
    val a = Variable("a").setType(t1)
    val b = Variable("b").setType(t2)
    val fv = Type.freshTypeVar
    val f = UnInterpretedFct("f", Some(Function(List(fv), Int)), List(fv))
    val form = Eq(Application(f, List(a)), Application(f, List(b)))
    val solver = Solver(QF_UF)
    assert( solver.test(form).get, "sat formula")
  }

}

