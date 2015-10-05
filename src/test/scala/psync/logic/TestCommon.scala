package psync.logic

import psync.formula._
import psync.utils.smtlib._
import dzufferey.utils.Logger

object TestCommon {

  val pid = CL.procType
  val n = CL.n
  val ho = CL.HO
  
  val i = Variable("i").setType(pid)
  val j = Variable("j").setType(pid)
  val k = Variable("k").setType(pid)

  def ite(a: Formula, b: Formula, c: Formula) = And(Or(Not(a), b), Or(a, c))
  
  /////////////
  // solving //
  /////////////

  //val cl = CL
  //val cl = ClFull
  val cl = new CL(Some(2), None, Some(1))

  val cl__0 = new CL(None, None, Some(0))
  val cl__1 = new CL(None, None, Some(1))
  val cl__2 = new CL(None, None, Some(2))
  val cl__3 = new CL(None, None, Some(3))
  val cl__4 = new CL(None, None, Some(4))
  
  val cl0_0 = new CL(Some(0), None, Some(0))
  val cl0_1 = new CL(Some(0), None, Some(1))
  val cl0_2 = new CL(Some(0), None, Some(2))
  val cl0_3 = new CL(Some(0), None, Some(3))
  val cl0_4 = new CL(Some(0), None, Some(4))
  
  val cl1_0 = new CL(Some(1), None, Some(0))
  val cl1_1 = new CL(Some(1), None, Some(1))
  val cl1_2 = new CL(Some(1), None, Some(2))
  val cl1_3 = new CL(Some(1), None, Some(3))
  val cl1_4 = new CL(Some(1), None, Some(4))

  val cl2_0 = new CL(Some(2), None, Some(0))
  val cl2_1 = new CL(Some(2), None, Some(1))
  val cl2_2 = new CL(Some(2), None, Some(2))
  val cl2_3 = new CL(Some(2), None, Some(3))
  val cl2_4 = new CL(Some(2), None, Some(4))

  val cl3_0 = new CL(Some(3), None, Some(0))
  val cl3_1 = new CL(Some(3), None, Some(1))
  val cl3_2 = new CL(Some(3), None, Some(2))
  val cl3_3 = new CL(Some(3), None, Some(3))
  val cl3_4 = new CL(Some(3), None, Some(4))

  def reduce(cl: CL, conjuncts: List[Formula], debug: Boolean): Formula = {
    if(debug) {
      Logger.moreVerbose
      Logger.moreVerbose
      Logger.disallow("Typer")
    }
    val c0 = conjuncts.flatMap(FormulaUtils.getConjuncts(_))
    val c1 = c0.map(Simplify.simplify)
    if(debug) {
      println("=======before reduce ")
      c1.foreach( f => println("  " + f) )
    }
    val f0 = And(c1 :_*)
    val f1 = cl.reduce(f0)
    if(debug) {
      Logger.lessVerbose
      Logger.lessVerbose
      Logger.allow("Typer")
      println("======= send to solver")
      FormulaUtils.getConjuncts(f1).foreach( f => println("  " + f) )
    }
    f1
  }

  def assertUnsat(conjuncts: List[Formula],
                  to: Long = 10000,
                  debug: Boolean = false,
                  reducer: CL = cl,
                  fname: Option[String] = None, 
                  useCvcMf: Boolean = false) {
    val f1 = reduce(reducer, conjuncts, debug)
    val solver = if (useCvcMf) Solver.cvc4mf(UFLIA, fname, to)
                 else Solver(UFLIA, fname, to)
    assert(!solver.testB(f1), "unsat formula")
  }

  def assertSat(conjuncts: List[Formula],
                to: Long = 10000,
                debug: Boolean = false,
                reducer: CL = cl,
                fname: Option[String] = None,
                useCvcMf: Boolean = false) {
    val f1 = reduce(cl, conjuncts, debug)
    val solver = if (useCvcMf) Solver.cvc4mf(UFLIA, fname, to)
                 else Solver(UFLIA, fname, to)
    assert( solver.testB(f1), "sat formula")
  }

  def getModel(conjuncts: List[Formula],
               to: Long = 10000,
               reducer: CL = cl,
               fname: Option[String] = None) {
    val f1 = reduce(reducer, conjuncts, true)
    val solver = Solver(UFLIA, fname, to)
    solver.testWithModel(f1) match {
      case Sat(Some(model)) =>
        Console.println(model.toString)
      case res =>
        assert( false, "could not parse model: " + res)
    }
  }

}
