package example

import psync._
import psync.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object ConsensusSelector {

  def apply(name: String,
            ops: RuntimeOptions,
            defaultHandler: Message => Unit,
            additionalOptions: Map[String, String]): Runtime[ConsensusIO, _] = name match {
    case "otr" | "" =>
      if (additionalOptions contains "after") {
        val after = additionalOptions("after").toInt
        new Runtime(new OTR(after), ops, defaultHandler)
      } else new Runtime(new OTR(), ops, defaultHandler)
    case "lv" => new Runtime(new LastVoting(ops.timeout), ops, defaultHandler)
    case "slv" => new Runtime(new ShortLastVoting(), ops, defaultHandler)
    case other =>
      Logger.logAndThrow("ConsensusSelector", Error, "unknown algorithm: " + other)
  }


}
