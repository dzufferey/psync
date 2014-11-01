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
    assert( solver.testB(form1), "sat formula")
    assert(!solver.testB(form2), "unsat formula")
  }

  test("qbf") {
    val form1 = Exists(List(b), And(a, Or(Not(a), b)))
    val form2 = ForAll(List(a), And(a, Or(Not(a), b)))
    val solver = Solver(UF)
    assert( solver.testB(form1), "sat formula")
    assert(!solver.testB(form2), "unsat formula")
  }

  test("qf_uf") {
    val form1 = And(pqp1,Not(pqp2))
    val form2 = And(Eq(p1,p2), And(pqp1,Not(pqp2)))
    val solver = Solver(QF_UF)
    assert( solver.testB(form1), "sat formula")
    assert(!solver.testB(form2), "unsat formula")
  }
  
  test("uf") {
    val ax = ForAll(List(p1), Eq(qp1,p2))
    val form1 = And(ax, pqp1)
    val form2 = And(ax, And(pqp1,Not(pp2)))
    val solver = Solver(UF)
    assert( solver.testB(form1), "sat formula")
    assert(!solver.testB(form2), "unsat formula")
  }

  test("qf_lia") {
    val form1 = And(Eq(x, y), Eq(Literal(0), Minus(x,y)))
    val form2 = And(Not(Eq(x, y)), Eq(Literal(0), Minus(x,y)))
    val solver = Solver(QF_LIA)
    assert( solver.testB(form1), "sat formula")
    assert(!solver.testB(form2), "unsat formula")
  }
  
  test("lia") {
    val form1 = ForAll(List(x), Or(Leq(x, Literal(0)), Lt(Literal(0), x)))
    val form2 = ForAll(List(x), Leq(x, Literal(0)))
    val solver = Solver(LIA)
    assert( solver.testB(form1), "sat formula")
    assert(!solver.testB(form2), "unsat formula")
  }

  test("qf_uflia") {
    val form1 = Eq(rqp1, rp2)
    val form2 = And(Eq(p2,qp3), Lt(rp2,rqp3))
    val solver = Solver(QF_UFLIA)
    assert( solver.testB(form1), "sat formula")
    assert(!solver.testB(form2), "unsat formula")
  }
  
  test("uflia") {
    val ax = ForAll(List(p1), Eq(qp1,p2))
    val form1 = And(ax, Eq(rqp1, rp2))
    val form2 = And(ax, Lt(rp2,rqp3))
    val solver = Solver(UFLIA)
    assert( solver.testB(form1), "sat formula")
    assert(!solver.testB(form2), "unsat formula")
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
    assert( solver.testB(form), "sat formula")
  }

  test("parsing model 1") {
    val t1 = UnInterpreted("T1")
    val t2 = UnInterpreted("T2")
    val a = Variable("a").setType(t1)
    val b = Variable("b").setType(t2)
    val fv = Type.freshTypeVar
    val f = UnInterpretedFct("f", Some(Function(List(fv), Int)), List(fv))
    val form = Eq(Application(f, List(a)), Application(f, List(b)))
    val solver = Solver(QF_UF)
    solver.testWithModel(form) match {
      case Sat(Some(model)) =>
        //Console.println(model.toString)
      case res =>
        assert( false, "could not parse model: " + res)
    }
  }

  test("parsing model 2") {
    val model = """(model 
  ;; universe for Set_ProcessID_:
  ;;   Set_ProcessID_!val!0 
  ;; -----------
  ;; definitions for universe elements:
  (declare-fun Set_ProcessID_!val!0 () Set_ProcessID_)
  ;; cardinality constraint:
  (forall ((x Set_ProcessID_)) (= x Set_ProcessID_!val!0))
  ;; -----------
  ;; universe for ProcessID:
  ;;   ProcessID!val!0 
  ;; -----------
  ;; definitions for universe elements:
  (declare-fun ProcessID!val!0 () ProcessID)
  ;; cardinality constraint:
  (forall ((x ProcessID)) (= x ProcessID!val!0))
  ;; -----------
  (define-fun A () Set_ProcessID_
    Set_ProcessID_!val!0)
  (define-fun i () ProcessID
    ProcessID!val!0)
  (define-fun v () Int
    2)
  (define-fun n () Int
    0)
  (define-fun decision ((x!1 ProcessID)) Int
    2)
  (define-fun x ((x!1 ProcessID)) Int
    2)
  (define-fun inProcessID ((x!1 ProcessID) (x!2 Set_ProcessID_)) Bool
    (= (x x!1) 2))
  (define-fun v__old__decided ((x!1 ProcessID)) Bool
    true)
  (define-fun decided ((x!1 ProcessID)) Bool
    false)
  (define-fun v__old__decision ((x!1 ProcessID)) Int
    1)
  (define-fun cardProcessID ((x!1 Set_ProcessID_)) Int
    0)
)
"""
    val cmds = Parser.parseModel(model).get
    val decls: List[(round.formula.Symbol, List[round.formula.Type])] = List(
      //(declare-fun cardProcessID (Set_ProcessID_) Int)
      (Cardinality, List(round.verification.Utils.procType)),
      //(declare-fun A () Set_ProcessID_)
      //(declare-fun n () Int)
      //(declare-fun decided (ProcessID) Bool)
      (UnInterpretedFct("decided"), Nil),
      //(declare-fun decision (ProcessID) Int)
      (UnInterpretedFct("decision"), Nil),
      //(declare-fun v () Int)
      //(declare-fun v__old__decided (ProcessID) Bool)
      (UnInterpretedFct("v__old__decided"), Nil),
      //(declare-fun i () ProcessID)
      //(declare-fun v__old__decision (ProcessID) Int)
      (UnInterpretedFct("v__old__decision"), Nil),
      //(declare-fun x (ProcessID) Int)
      (UnInterpretedFct("x"), Nil),
      //(declare-fun inProcessID (ProcessID Set_ProcessID_) Bool)
      (In, List(round.verification.Utils.procType))
    )
    val parsed = Model(cmds, decls) 
    ()
  }

}

