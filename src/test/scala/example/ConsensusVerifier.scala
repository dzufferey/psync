package example

import round._
import round.verification._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.arg._

object ConsensusVerifier extends round.utils.DefaultOptions {
  
  var v = 3
  newOption("-n", Int( i => v = i), "Ort1/2/3")
  
  var lv = false
  newOption("-lv", Unit( () => lv = true), "LastVoting")

  var r = "report.html"
  newOption("-r", String( i => r = i), "report.html")

  newOption("-dumpVcs", Unit( () => round.utils.Options.dumpVcs = true), "dump the SMT queries into files")

  val usage = "..."

  def main(args: Array[java.lang.String]) {
    Logger.moreVerbose
    apply(args)

    val dummyIO = new ConsensusIO {
      val initialValue = 0
      def decide(value: scala.Int) { }
    }

    val alg =
      if (lv) new LastVoting()
      else
        v match {
          case 1 => new OTR()
          case 2 => new OTR2()
          case 3 => new OTR3()
          case _ => sys.error("unknown OTR version")
        }

    val verifer = new Verifier(alg, dummyIO)

    Logger("OtrVerifier", Notice, "verifying ...")
    val report = verifer.check
    Logger("OtrVerifier", Notice, "saving verification report as " + r)
    report.makeHtmlReport(r)
  }

}
