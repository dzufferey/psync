package round.utils

import dzufferey.arg._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

/** default configuration object */
object Options extends Options {
 
  //verbosity
  newOption("-v", Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", Unit(() => Logger.lessVerbose), "decrease the verbosity level.")
  newOption("--hide", String( str => Logger.disallow(str)), "hide the output with given prefix.")
  newOption("--noAssert", Unit(() => Logger.disableAssert), "remove some assertions.")
 
  //general reporting option
  var report = false
  var reportOutput: Option[java.lang.String] = None
  var stats = false
 
  newOption("-r", Unit(() => report = true), "output a report (with a default name).")
  newOption("--report", String(str => { report = true; reportOutput = Some(str) } ), "output a report with given name.")
  newOption("--stats", Unit(() => stats = true), "print statistics about the execution.")
 
  //general config stuff
  //var maxChildren = -1
  //newOption("--maxChildren", Arg.Int ( i => maxChildren = i), "limit the number of children that can be spawned at the same time (default: no limit).")
 
  var dumpVcs = false

  newOption("--smtSolver", String(str => smtlib.Solver.setCmd(str.split(" "))), "The smt sovler (+ options) to use (default: \"z3 -smt2 -in\").")
  newOption("--dumpVcs", Unit( () => dumpVcs = true ), "dump the SMT queries into files.")
 
  val usage = "..."

}
