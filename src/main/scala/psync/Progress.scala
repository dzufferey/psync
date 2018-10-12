package psync

class Progress(val value: Long) extends AnyVal {

  def isGoAhead = this == Progress.goAhead
  def isUnchanged = this == Progress.unchanged
  def isWaitMessage = Progress.isWaitMessage(this)
  def isTimeout = Progress.isTimeout(this)
  def isStrict = Progress.isStrict(this)

  def timeout: Long = Progress.getTimeout(this)

  override def toString = {
    if (isWaitMessage) {
      if (isStrict) "StrictWaitForMessage"
      else "WaitForMessage"
    } else if (isTimeout) {
      if (isStrict) "StrictTimeout(" + timeout + ")"
      else "Timeout(" + timeout + ")"
    } else if (isGoAhead) {
      "GoAhead"
    } else if (isUnchanged) {
      "Unchanged"
    } else {
      sys.error("invalid Progress value: " + value)
    }
  }

  def asBits = java.lang.Long.toBinaryString(value)

}

object Progress {

  //use the top two bits for extra info
  //the 1st top bit is TO vs other
  //the 2nd top bit is (non)strict

  private final val mask = -1l >>> 2

  @inline final def isWaitMessage(p: Progress): Boolean = (p.value & (1l << 63)) != 0 && (p.value & mask) == 4
  @inline final def isTimeout(p: Progress): Boolean     = (p.value & (1l << 63)) == 0
  @inline final def isStrict(p: Progress): Boolean      = (p.value & (1l << 62)) != 0

  final val waitMessage         = new Progress( (1l << 63) | 4 )
  final val strictWaitMessage   = new Progress( (3l << 62) | 4 )
  final val goAhead             = new Progress( (1l << 63) | 2 )
  final val unchanged           = new Progress( (1l << 63) | 1 )

  @inline final def timeout(millis: Long) = new Progress( millis & mask )
  @inline final def strictTimeout(millis: Long) = new Progress( (millis & mask) | (1l << 62) )

  @inline final def getTimeout(p: Progress): Long = {
    assert(isTimeout(p))
    val noMask = (p.value & mask)
    (noMask << 2) >> 2 //to get the sign back
  }

  final def timeoutInBounds(l: Long) = l == ((l << 2) >> 2)

  //TODO
  // default timeout (from config)
  // offset timeout (add/sub a constant from the default timeout)
  // factor timeout (multiply the default timeout)

}
