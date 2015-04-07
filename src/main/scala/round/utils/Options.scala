package round.utils

import dzufferey.arg._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

/** default configuration object */
abstract class DefaultOptions extends Options {

  //verbosity
  newOption("-v", Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", Unit(() => Logger.lessVerbose), "decrease the verbosity level.")
  newOption("--hide", String( str => Logger.disallow(str)), "hide the output with given prefix.")
  newOption("--noAssert", Unit(() => Logger.disableAssert), "remove some assertions.")
 
  //verification
  newOption("--smtSolver", String(str => smtlib.Solver.setCmd(str.split(" "))), "[verification] The smt sovler (+ options) to use (default: \"z3 -smt2 -in\").")
  newOption("--dumpVcs", Unit( () => Options.dumpVcs = true ), "[verification] dump the SMT queries into files.")
  newOption("--timeout", Int( i => Options.timeout = i.toLong * 1000 ), "[verification] timeout for the SMT solver is second (default: 10).")
 
}

object Options {
  
  //var stats = false
  //newOption("--stats", Unit(() => Options.stats = true), "print statistics about the execution.")
 
  //verification
  var dumpVcs = false
  var timeout = 10000l
 
}
