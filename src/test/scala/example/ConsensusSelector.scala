package example

import round._
import round.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object ConsensusSelector {

  def apply(name: String, additionalOptions: Map[String, String]): Algorithm[ConsensusIO, _] = name match {
    case "otr" | "" =>
      if (additionalOptions contains "after") {
        val after = additionalOptions("after").toInt
        new OTR(after)
      } else new OTR()
    //case "lv" => new LastVoting2()
    //case "slv" => new ShortLastVoting()
    case other =>
      Logger.logAndThrow("ConsensusSelector", Error, "unknown algorithm: " + other)
  }

  def apply(name: String,
            ops: RuntimeOptions,
            defaultHandler: Message => Unit,
            additionalOptions: Map[String, String]): Runtime[ConsensusIO, _] = name match {
    case "otr" | "" =>
      if (additionalOptions contains "after") {
        val after = additionalOptions("after").toInt
        new Runtime(new OTR(after), ops, defaultHandler)
      } else new Runtime(new OTR(), ops, defaultHandler)
    //case "lv" => new Runtime(new LastVoting2(), ops, defaultHandler)
    //case "slv" => new Runtime(new ShortLastVoting() ops, defaultHandler)
    case other =>
      Logger.logAndThrow("ConsensusSelector", Error, "unknown algorithm: " + other)
  }


}
