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
