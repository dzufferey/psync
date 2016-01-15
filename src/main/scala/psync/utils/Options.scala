package psync.utils

import dzufferey.arg._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

/** default configuration object */
abstract class DefaultOptions extends Options {

  //verbosity
  newOption("-v", Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", Unit(() => Logger.lessVerbose), "decrease the verbosity level.")
  newOption("--hide", String( str => Logger.disallow(str)), "hide the output with given prefix.")
 
}

object Options {
  
  //var stats = false
  //newOption("--stats", Unit(() => Options.stats = true), "print statistics about the execution.")
 
  //verification
  var dumpVcs = false
  var timeout = 10000l
  var logQI = false
 
}

abstract class VerificationOptions extends DefaultOptions {

  //verification
  newOption("--smtSolver", String(str => smtlib.Solver.setCmd(str.split(" "))), "The smt sovler (+ options) to use (default: \"z3 -smt2 -in\").")
  newOption("--dumpVcs", Unit( () => Options.dumpVcs = true ), "dump the SMT queries into files.")
  newOption("--timeout", Int( i => Options.timeout = i.toLong * 1000 ), "timeout for the SMT solver is second (default: 10).")
  newOption("--cvc4mf", Unit( () => smtlib.Solver.useCvc4Mf ), "use cvc4 (model finder engine) as smt sovler.")
  newOption("--z3", Unit( () => smtlib.Solver.useZ3 ), "use z3 as smt sovler (default).")
  newOption("--logQI", Unit( () => Options.logQI = true ), "log quantifier instantition (default false).")

}

