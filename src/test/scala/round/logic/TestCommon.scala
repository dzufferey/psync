package round.logic

import round.formula._
import round.utils.smtlib._
import dzufferey.utils.Logger

object TestCommon {

  //val cl = CL
  //val cl = ClFull
  val cl = new CL(Some(2), None)

  def reduce(conjuncts: List[Formula], debug: Boolean): Formula = {
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

  def assertUnsat(conjuncts: List[Formula], to: Long = 10000, debug: Boolean = false, fname: Option[String] = None, useCvcMf: Boolean = false) {
    val f1 = reduce(conjuncts, debug)
    val solver = if (useCvcMf) Solver.cvc4mf(UFLIA, fname, to)
                 else Solver(UFLIA, fname, to)
    assert(!solver.testB(f1), "unsat formula")
  }

  def assertSat(conjuncts: List[Formula], to: Long = 10000, debug: Boolean = false, fname: Option[String] = None, useCvcMf: Boolean = false) {
    val f1 = reduce(conjuncts, debug)
    val solver = if (useCvcMf) Solver.cvc4mf(UFLIA, fname, to)
                 else Solver(UFLIA, fname, to)
    assert( solver.testB(f1), "sat formula")
  }

  def getModel(conjuncts: List[Formula], to: Long = 10000) {
    val f1 = reduce(conjuncts, true)
    val solver = Solver(UFLIA, None, to)
    solver.testWithModel(f1) match {
      case Sat(Some(model)) =>
        Console.println(model.toString)
      case res =>
        assert( false, "could not parse model: " + res)
    }
  }

}
