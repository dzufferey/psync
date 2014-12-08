package example

import round._
import round.verification._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.arg._

object ConsensusVerifier extends round.utils.DefaultOptions {
  
  var v = 1
  newOption("-n", Int( i => v = i), "1/2/3")
  
  var lv = false
  newOption("-lv", Unit( () => lv = true), "LastVoting (default OTR)")

  var r = "report.html"
  newOption("-r", String( i => r = i), "report.html")

  newOption("-dumpVcs", Unit( () => round.utils.Options.dumpVcs = true), "dump the SMT queries into files")

  val usage = "..."

  def main(args: Array[java.lang.String]) {
    Logger.moreVerbose
    apply(args)

    val alg = v match {
        case 1 => if (lv) new LastVoting() else new OTR()
        case 2 => if (lv) new LastVoting2() else new OTR2()
        case 3 => if (lv) new LastVoting3() else new OTR3()
        case _ => sys.error("unknown version")
      }

    val verifer = new Verifier(alg)

    Logger("ConsensusVerifier", Notice, "verifying ...")
    val report = verifer.check
    Logger("ConsensusVerifier", Notice, "saving verification report as " + r)
    report.makeHtmlReport(r)
  }

}
