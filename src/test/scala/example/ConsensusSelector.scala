package example

import round._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object ConsensusSelector {

  def apply(name: String, additionalOptions: Map[String, String]): Algorithm[ConsensusIO] = name match {
    case "otr" | "" =>
      if (additionalOptions contains "after") {
        val after = additionalOptions("after").toInt
        new OTR(after)
      } else new OTR()
    case "lv" => new LastVoting2()
    case "slv" => new ShortLastVoting()
    case other =>
      Logger.logAndThrow("ConsensusSelector", Error, "unknown algorithm: " + other)
  }


}
