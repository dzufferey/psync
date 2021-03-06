package psync.logic

import psync.formula._
import psync.logic.quantifiers._
import psync.utils.SmtSolver
import psync.verification.VerificationOptions
import dzufferey.smtlib.{Sat, UnSat, Model}
import dzufferey.utils.{Logger, Namer}

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
  
  def expandDisj(f: Formula)(implicit namer: Namer): Seq[Formula] = {
    val f2 = Simplify.nnf(f)
    val (f3, _) = quantifiers.getExistentialPrefix(f2)
    Simplify.dnf(f3) match {
      case Or(lst @ _*) => lst
      case other => List(other)
    }
  }

  def reduce(reducer: ClReducer, conjuncts: List[Formula], debug: Boolean): Formula = {
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
      val f1 = reducer.reduce(f0)
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
                  dnfExpansion: Boolean = false,
                  onlyAxioms: Boolean = false,
                  fname: Option[String] = None,
                  useCvcMf: Boolean = false): Unit = {
    val r = if (onlyAxioms) new ClAxiomatized(reducer) else new CL(reducer)
    implicit val namer = r.namer
    val fAll = And(conjuncts:_*)
    val fs = if (dnfExpansion) expandDisj(fAll) else List(fAll)
    fs.foreach( f => {
      val f0 = reduce(r, List(f), debug)
      val f1 = SmtSolver.convert(SmtSolver.uninterpretSymbols(f0))
      val solver = if (useCvcMf) SmtSolver.cvc4mf(fname, to)
                   else SmtSolver.z3(fname, to)
      try {
        assert(!solver.testB(f1), "unsat formula")
      } catch {
        case t: Throwable => t.printStackTrace
          throw t
      }
    })
  }

  def assertUnsat(conjuncts: List[Formula],
                  reducer: ClConfig): Unit = {
    assertUnsat(conjuncts, 10000, false, reducer)
  }

  def assertSat(conjuncts: List[Formula],
                to: Long = 10000,
                debug: Boolean = false,
                reducer: ClConfig = cl,
                dnfExpansion: Boolean = false,
                onlyAxioms: Boolean = false,
                fname: Option[String] = None,
                useCvcMf: Boolean = false): Unit = {
    val r = if (onlyAxioms) new ClAxiomatized(reducer) else new CL(reducer)
    implicit val namer = r.namer
    val fAll = And(conjuncts:_*)
    val fs = if (dnfExpansion) expandDisj(fAll) else List(fAll)
    assert(fs.exists( f => {
      val f0 = reduce(r, List(f), debug)
      val f1 = SmtSolver.convert(SmtSolver.uninterpretSymbols(f0))
      val solver = if (useCvcMf) SmtSolver.cvc4mf(fname, to)
                   else SmtSolver.z3(fname, to)
      try {
        solver.testB(f1)
      } catch {
        case t: Throwable => t.printStackTrace
          throw t
      }
    }), "sat formula")
  }

  def assertSat(conjuncts: List[Formula],
                reducer: ClConfig): Unit = {
    assertSat(conjuncts, 10000, false, reducer)
  }


  def getModel(conjuncts: List[Formula],
               to: Long = 10000,
               reducer: ClConfig = cl,
               dnfExpansion: Boolean = false,
               onlyAxioms: Boolean = false,
               fname: Option[String] = None,
               useCvcMf: Boolean = false): Unit = {
    val r = if (onlyAxioms) new ClAxiomatized(reducer) else new CL(reducer)
    implicit val namer = r.namer
    val fAll = And(conjuncts:_*)
    val fs = if (dnfExpansion) expandDisj(fAll) else List(fAll)
    val acc: Option[Model] = None
    val mdl = fs.foldLeft(acc)( (acc, f) => acc.orElse({
      val f0 = reduce(r, List(f), true)
      val f1 = SmtSolver.convert(SmtSolver.uninterpretSymbols(f0))
      val solver = if (useCvcMf) SmtSolver.cvc4mf(fname, to)
                   else SmtSolver.z3(fname, to)
      val conjs = dzufferey.smtlib.FormulaUtils.getConjuncts(f1)
      conjs.foreach(solver.assert)
      solver.checkSat match {
        case Sat(_) =>
          solver.getPartialModel.orElse(sys.error("failed to get a model"))
        case UnSat =>
          None
        case err =>
          sys.error("not sat: " + err)
      }
    }))
    mdl match {
      case Some(model) => Console.println(model.toString)
      case None => sys.error("unsat")
    }
  }

}
