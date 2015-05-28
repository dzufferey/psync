package round.logic

import round.formula._
import round.utils.smtlib._
import dzufferey.utils.Logger

object TestCommon {

  def assertUnsat(conjuncts: List[Formula]) {
    val c0 = conjuncts.map(Simplify.simplify)
    val f0 = And(c0 :_*)
    val f1 = CL.reduce(f0)
    val solver = Solver(UFLIA)
    assert(!solver.testB(f1), "unsat formula")
  }
  
  def assertUnsatDebug(conjuncts: List[Formula]) {
    Logger.moreVerbose
    Logger.moreVerbose
    Logger.disallow("Typer")
    val c0 = conjuncts.map(Simplify.simplify)
    println("=======before reduce ")
    c0.foreach( f => println("  " + f) )
    val f0 = And(c0 :_*)
    val f1 = CL.reduce(f0)
    println("======= send to solver")
    FormulaUtils.getConjuncts(f1).foreach( f => println("  " + f) )
    //val solver = Solver(UFLIA, "test.smt2")
    //val solver = Solver.cvc4mf(UFLIA, None, 600000)
    val solver = Solver(UFLIA)
    Logger.lessVerbose
    Logger.lessVerbose
    Logger.allow("Typer")
    assert(!solver.testB(f1), "unsat formula")
  }

  def assertSat(conjuncts: List[Formula]) {
    val c0 = conjuncts.map(Simplify.simplify)
    val f0 = And(c0 :_*)
    val f1 = CL.reduce(f0)
    val solver = Solver(UFLIA)
    assert( solver.testB(f1), "sat formula")
  }

  def assertSatDebug(conjuncts: List[Formula]) {
    Logger.moreVerbose
    Logger.moreVerbose
    Logger.disallow("Typer")
    val c0 = conjuncts.map(Simplify.simplify)
    //Simplify is in src/main/scala/round/formula/simplify  
    val f0 = And(c0 :_*)
    // this call apply from formula in class symbol line 93
    println("=======before reduce ")
    c0.foreach( f => println("  " + f) )
    val f1 = CL.reduce(f0)
    println("======= send to solver")
    FormulaUtils.getConjuncts(f1).foreach( f => println("  " + f) )
    //val solver = Solver(UFLIA, "test.smt2")
    //val solver = Solver.cvc4mf(UFLIA, None, 600000)
    val solver = Solver(UFLIA)
    Logger.lessVerbose
    Logger.lessVerbose
    Logger.allow("Typer")
    assert( solver.testB(f1), "sat formula")
  }

}
