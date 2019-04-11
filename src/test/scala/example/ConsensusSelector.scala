package example

import psync._
import psync.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object ConsensusSelector {

  def apply(name: String,
            rt: Runtime,
            additionalOptions: Map[String,Any]): Algorithm[ConsensusIO, _] = {
    val ops = rt.options
    name match {
      case "otr" | "" =>
        if (additionalOptions contains "after") {
          val after = additionalOptions("after").asInstanceOf[Int]
          new OTR(rt, ops.timeout,after)
        } else {
          new OTR(rt, ops.timeout)
        }
      case "lv" =>
        if (additionalOptions contains "sync") {
          val sync = additionalOptions("sync").asInstanceOf[SyncCondition.SyncCondition]
          new LastVoting(rt, ops.timeout, sync)
        } else {
          new LastVoting(rt, ops.timeout)
        }
      case "lve" => new LastVotingEvent(rt, ops.timeout)
      case "slv" => new ShortLastVoting(rt, ops.timeout)
      case other =>
        Logger.logAndThrow("ConsensusSelector", Error, "unknown algorithm: " + other)
    }
  }

}
