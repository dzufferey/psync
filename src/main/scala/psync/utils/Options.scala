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
 
  //stats
  private var stats = false
  private def setStatsHook: scala.Unit = {
    if (!stats) {
      stats = true
      java.lang.Runtime.getRuntime().addShutdownHook(new Thread() {
        override def run: scala.Unit = { Logger("Stats", Notice, Stats.toString) }
      })
    }
  }
  newOption("--stat", Unit(() => setStatsHook ), "print some statistics on exit")

}
