package psync.runtime

class Progress(val timeout: Long) extends AnyVal {

  def isWaitMessage = this == Progress.waitMessage
  def isGoAhead = this == Progress.goAhead
  def isUnchanged = this == Progress.unchanged
  def isTimeout = this.timeout > Progress.waitMessage.timeout

}

object Progress {

  final val waitMessage = new Progress( Long.MinValue + 3000 )
  final val goAhead = new Progress( Long.MinValue + 2000 )
  final val unchanged = new Progress( Long.MinValue + 1000 )
  @inline final def timeout(millis: Long) = {
    assert(millis > waitMessage.timeout)
    new Progress( millis )
  }

  //TODO
  // default timeout (from config)
  // offset timeout (add/sub a constant from the default timeout)
  // factor timeout (multiply the default timeout)

}
