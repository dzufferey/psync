package psync.verification

import psync.utils.DefaultOptions
import psync.utils.SmtSolver

import dzufferey.arg._

object VerificationOptions {
  
  var dumpVcs = false
  var logQI = false
 
}

abstract class VerificationOptions extends DefaultOptions {

  //verification
  newOption("--smtSolver", String(str => SmtSolver.setCmd(str.split(" "))), "The smt sovler (+ options) to use (default: \"z3 -smt2 -in\").")
  newOption("--dumpVcs", Unit( () => VerificationOptions.dumpVcs = true ), "dump the SMT queries into files.")
  newOption("--timeout", Int( i => SmtSolver.defaultTO(i * 1000) ), "timeout for the SMT solver is second (default: 10).")
  newOption("--cvc4mf", Unit( () => SmtSolver.useCvc4Mf ), "use cvc4 (model finder engine) as smt sovler.")
  newOption("--z3", Unit( () => SmtSolver.useZ3 ), "use z3 as smt sovler (default).")
  newOption("--logQI", Unit( () => VerificationOptions.logQI = true ), "log quantifier instantition (default false).")

}
