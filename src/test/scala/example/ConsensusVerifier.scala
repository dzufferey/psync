package example

import round._
import round.verification._
import round.utils.{Logger, Arg, Options}
import round.utils.LogLevel._

object ConsensusVerifier extends Options {
  
  newOption("-v", Arg.Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", Arg.Unit(() => Logger.lessVerbose), "decrease the verbosity level.")
  
  var v = 3
  newOption("-n", Arg.Int( i => v = i), "Ort1/2/3")
  
  var lv = false
  newOption("-lv", Arg.Unit( () => lv = true), "LastVoting")

  var r = "report.html"
  newOption("-r", Arg.String( i => r = i), "report.html")

  val usage = "..."

  def main(args: Array[String]) {
    Logger.moreVerbose
    apply(args)

    val dummyIO = new ConsensusIO {
      val initialValue = 0
      def decide(value: Int) { }
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
