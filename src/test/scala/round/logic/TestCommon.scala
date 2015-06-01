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
      println("======= send to solver")
      FormulaUtils.getConjuncts(f1).foreach( f => println("  " + f) )
      Logger.lessVerbose
      Logger.lessVerbose
      Logger.allow("Typer")
    }
    f1
  }

  def assertUnsat(conjuncts: List[Formula], to: Long = 10000, debug: Boolean = false, fname: Option[String] = None) {
    val f1 = reduce(conjuncts, debug)
    //val solver = Solver.cvc4mf(UFLIA, fname, to)
    val solver = Solver(UFLIA, fname, to)
    assert(!solver.testB(f1), "unsat formula")
  }

  def assertSat(conjuncts: List[Formula], to: Long = 10000, debug: Boolean = false, fname: Option[String] = None) {
    val f1 = reduce(conjuncts, debug)
    //val solver = Solver.cvc4mf(UFLIA, fname, to)
    val solver = Solver(UFLIA, fname, to)
    assert( solver.testB(f1), "sat formula")
  }

}
