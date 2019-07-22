package psync

class Progress(val value: Long) extends AnyVal {

  def isGoAhead = this == Progress.goAhead
  def isUnchanged = this == Progress.unchanged
  def isWaitMessage = Progress.isWaitMessage(this)
  def isTimeout = Progress.isTimeout(this)
  def isStrict = Progress.isStrict(this)

  def timeout: Long = Progress.getTimeout(this)

  def orElse(p: Progress) = Progress.orElse(this, p)
  def lub(p: Progress) = Progress.lub(this, p)
  def glb(p: Progress) = Progress.glb(this, p)

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

  @inline final def orElse(p1: Progress, p2: Progress): Progress = if (p1 != unchanged) p1 else p2

  //max timeout and strictness
  @inline final def lub(p1: Progress, p2: Progress): Progress = {
    assert(p1 != unchanged && p2 != unchanged)
    val strict = isStrict(p1) || isStrict(p2)
    if (isWaitMessage(p1) || isWaitMessage(p2)) {
      if (strict) strictWaitMessage else waitMessage
    } else if (p1 == goAhead) {
      p2
    } else if (p2 == goAhead) {
      p1
    } else {
      val to1 = getTimeout(p1)
      val to2 = getTimeout(p2)
      val to = math.max(to1, to2)
      if (strict) strictTimeout(to) else timeout(to)
    }
  }

  //min timeout and strictness
  @inline final def glb(p1: Progress, p2: Progress): Progress = {
    assert(p1 != unchanged && p2 != unchanged)
    val strict = isStrict(p1) && isStrict(p2)
    if (p1 == goAhead || p2 == goAhead) {
      goAhead
    } else if (isWaitMessage(p1) && isWaitMessage(p2)) {
      if (strict) strictWaitMessage else waitMessage
    } else {
      val to1 = if (isTimeout(p1)) getTimeout(p1) else getTimeout(p2)
      val to2 = if (isTimeout(p2)) getTimeout(p2) else to1
      val to = math.min(to1, to2)
      if (strict) strictTimeout(to) else timeout(to)
    }
  }

  final def timeoutInBounds(l: Long) = l == ((l << 2) >> 2)

  //TODO
  // default timeout (from config)
  // offset timeout (add/sub a constant from the default timeout)
  // factor timeout (multiply the default timeout)

}
