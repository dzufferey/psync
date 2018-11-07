package psync.logic

import psync.formula._
import psync.logic.quantifiers._
import psync.utils.SmtSolver
import psync.verification.VerificationOptions
import dzufferey.smtlib.Sat
import dzufferey.utils.Logger

object TestCommon {

  val pid = CL.procType
  val n = CL.n
  val ho = CL.HO

  val i = Variable("i").setType(pid)
  val j = Variable("j").setType(pid)
  val k = Variable("k").setType(pid)
  val l = Variable("l").setType(pid)
  val m = Variable("m").setType(pid)

  def ite(a: Formula, b: Formula, c: Formula) = And(Or(Not(a), b), Or(a, c))

  /////////////
  // solving //
  /////////////

  val cl = ClDefault

  val clProc = ClProc

  def cln(v: Int, t: Tactic, local: Boolean) = ClConfig(Some(v), None, QStrategy(t, local))

  val c1e1 = cln(1, new Eager(1), true)
  val c1e2 = cln(1, new Eager(2), true)
  val c2e1 = cln(2, new Eager(1), true)
  val c2e2 = cln(2, new Eager(2), true)
  val c3e1 = cln(3, new Eager(1), true)
  val c3e2 = cln(3, new Eager(2), true)
  val c3e3 = cln(3, new Eager(3), true)

  def reduce(clc: ClConfig, conjuncts: List[Formula], debug: Boolean): Formula = {
    val cl = new CL(clc)
    if(debug) {
      Logger.moreVerbose //linter:ignore IdenticalStatements
      Logger.moreVerbose
      Logger.disallow("Typer")
      VerificationOptions.logQI = true
    }
    try {
      val c0 = conjuncts.flatMap(FormulaUtils.getConjuncts(_))
      val c1 = c0.map(Simplify.simplify)
      if(debug) {
        println("=======before reduce ")
        c1.foreach( f => println("  " + f) )
        c1.foreach( f => println("  " + f.toStringFull) )
      }
      val f0 = And(c1 :_*)
      val f1 = cl.reduce(f0)
      if(debug) {
        println("======= send to solver")
        FormulaUtils.getConjuncts(f1).foreach( f => println("  " + f) )
      }
      f1
    } catch {
      case t: Throwable => t.printStackTrace
        throw t
    } finally {
      if(debug) {
        Logger.lessVerbose //linter:ignore IdenticalStatements
        Logger.lessVerbose
        Logger.allow("Typer")
        VerificationOptions.logQI = false
      }
    }
  }

  def assertUnsat(conjuncts: List[Formula],
                  to: Long = 10000,
                  debug: Boolean = false,
                  reducer: ClConfig = cl,
                  fname: Option[String] = None,
                  useCvcMf: Boolean = false) {
    val f0 = reduce(reducer, conjuncts, debug)
    val f1 = SmtSolver.convert(SmtSolver.uninterpretSymbols(f0))
    val solver = if (useCvcMf) SmtSolver.cvc4mf(fname, to)
                 else SmtSolver.z3(fname, to)
    try {
      assert(!solver.testB(f1), "unsat formula")
    } catch {
      case t: Throwable => t.printStackTrace
        throw t
    }
  }

  def assertUnsat(conjuncts: List[Formula],
                  reducer: ClConfig) {
    assertUnsat(conjuncts, 10000, false, reducer)
  }

  def assertSat(conjuncts: List[Formula],
                to: Long = 10000,
                debug: Boolean = false,
                reducer: ClConfig = cl,
                fname: Option[String] = None,
                useCvcMf: Boolean = false) {
    val f0 = reduce(reducer, conjuncts, debug)
    val f1 = SmtSolver.convert(SmtSolver.uninterpretSymbols(f0))
    val solver = if (useCvcMf) SmtSolver.cvc4mf(fname, to)
                 else SmtSolver.z3(fname, to)
    assert( solver.testB(f1), "sat formula")
  }

  def assertSat(conjuncts: List[Formula],
                reducer: ClConfig) {
    assertSat(conjuncts, 10000, false, reducer)
  }


  def getModel(conjuncts: List[Formula],
               to: Long = 10000,
               reducer: ClConfig = cl,
               fname: Option[String] = None,
               useCvcMf: Boolean = false) {
    val f0 = reduce(reducer, conjuncts, true)
    val f1 = SmtSolver.convert(SmtSolver.uninterpretSymbols(f0))
    val solver = if (useCvcMf) SmtSolver.cvc4mf(fname, to)
                 else SmtSolver.z3(fname, to)
    /*
    solver.testWithModel(f1) match {
      case Sat(Some(model)) =>
        Console.println(model.toString)
      case res =>
        assert( false, "could not parse model: " + res)
    }
    */
    val conjs = dzufferey.smtlib.FormulaUtils.getConjuncts(f1)
    conjs.foreach(solver.assert)
    solver.checkSat match {
      case Sat(_) =>
        solver.getPartialModel match {
          case Some(model) =>
            Console.println(model.toString)
          case None =>
            sys.error("failed to get a model")
        }
      case err =>
        sys.error("not sat: " + err)
    }
  }

}
