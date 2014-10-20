package round.utils.smtlib

import round.formula._

import org.scalatest._

class SolverSuite extends FunSuite {

  //import dzufferey.utils.Logger
  //Logger.moreVerbose
  //Logger.moreVerbose

  import round.formula.Common._

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

  test("parsing model") {
    val t1 = UnInterpreted("T1")
    val t2 = UnInterpreted("T2")
    val a = Variable("a").setType(t1)
    val b = Variable("b").setType(t2)
    val fv = Type.freshTypeVar
    val f = UnInterpretedFct("f", Some(Function(List(fv), Int)), List(fv))
    val form = Eq(Application(f, List(a)), Application(f, List(b)))
    val solver = Solver(QF_UF)
    solver.assert(form)
    assert( solver.checkSat.get, "sat formula")
    val model = solver.getModel
    assert( model.isDefined, "could not parse model")
    //Console.println(model.toString)
  }

}

