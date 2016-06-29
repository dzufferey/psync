package example

import psync._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.arg._

object Verifier extends psync.verification.VerificationOptions {
  
  var v = 1
  newOption("-n", Int( i => v = i), "1/2")
  
  var lv = false
  newOption("-lv", Unit( () => lv = true), "LastVoting (default OTR)")

  var r = "report.html"
  newOption("-r", String( i => r = i), "report name (default: report.html)")

  val usage = "Give the name of the class to verify as argument"

  def main(args: Array[java.lang.String]) {
    Logger.moreVerbose
    apply(args)

    val alg = input match {
      case x :: _ => x
      case Nil => "example." + (if (lv) "LastVoting" else "OTR") + (if (v == 1) "" else v)
    }
    val verifer = psync.verification.Verifier(alg)

    Logger("ConsensusVerifier", Notice, "verifying ...")
    val report = verifer.check
    Logger("ConsensusVerifier", Notice, "saving verification report as " + r)
    report.makeHtmlReport(r)
  }

}
