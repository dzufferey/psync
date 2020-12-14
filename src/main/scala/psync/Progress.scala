package psync


/** Tells the runtime how to behave. Please use the constructor of the Progress object.
 *
 * A Progress value can be of different types:
 * - timeout
 * - wait
 * - goAhead
 * - sync
 * - unchanged
 *
 * "wait" and "timeout" can be "strict" which means catching-up is disabled.
 * Catching-up occurs on f+1 processes at an higher round.
 *
 * "sync" overrides the default behaviors and "sync(k)" checks that k correct processes
 * are at the current round or higher. "sync" is always strict.
 * Sync is needed when dealing with Byzantine faults and unreliable communication.
 * (In the current implementation, it is possible that some messages are dropped
 * even if the communication is reliable!)
 *
 */
class Progress(val value: Long) extends AnyVal {

  def isGoAhead     = Progress.isGoAhead(this)
  def isUnchanged   = Progress.isUnchanged(this)
  def isWaitMessage = Progress.isWaitMessage(this)
  def isTimeout     = Progress.isTimeout(this)
  def isSync        = Progress.isSync(this)
  def isStrict      = Progress.isStrict(this)

  // for TO
  def timeout: Long = Progress.getTimeout(this)
  // for sync
  def k: Int = Progress.getTimeout(this).toInt

  def orElse(p: Progress) = Progress.orElse(this, p)
  def lub(p: Progress) = Progress.lub(this, p)
  def glb(p: Progress) = Progress.glb(this, p)

  override def toString = {
    if (isWaitMessage) {
      if (isStrict) "StrictWaitForMessage"
      else "WaitForMessage"
    } else if (isTimeout) {
      if (isStrict) s"StrictTimeout($timeout)"
      else s"Timeout($timeout)"
    } else if (isGoAhead) {
      "GoAhead"
    } else if (isUnchanged) {
      "Unchanged"
    } else if (isSync) {
      s"Sync($k)"
    } else {
      sys.error("invalid Progress value: " + value)
    }
  }

  def asBits = java.lang.Long.toBinaryString(value)

}

object Progress {

  //use the top three bits for extra info
  //top 2 bits are the type
  //3rd bit is strict

  private final val nMaskBits = 3
  private final val valueMask = -1L >>> nMaskBits
  private final val headerMask = 7L << (64 - nMaskBits)

  private final val timeoutHeader         = 0L << (64 - nMaskBits)
  private final val timeoutStrictHeader   = 1L << (64 - nMaskBits)
  private final val waitHeader            = 2L << (64 - nMaskBits)
  private final val waitStrictHeader      = 3L << (64 - nMaskBits)
  private final val goAheadHeader         = 4L << (64 - nMaskBits)
  private final val syncHeader            = 5L << (64 - nMaskBits)
  private final val unchangedHeader       = 6L << (64 - nMaskBits)

  @inline private final def header(p: Progress): Long = (p.value & headerMask)

  @inline final def isWaitMessage(p: Progress): Boolean = header(p) == waitHeader    || header(p) == waitStrictHeader
  @inline final def isTimeout(p: Progress): Boolean     = header(p) == timeoutHeader || header(p) == timeoutStrictHeader
  @inline final def isSync(p: Progress): Boolean        = header(p) == syncHeader
  @inline final def isGoAhead(p: Progress): Boolean     = header(p) == goAheadHeader
  @inline final def isUnchanged(p: Progress): Boolean   = header(p) == unchangedHeader
  @inline final def isStrict(p: Progress): Boolean      = (header(p) & timeoutStrictHeader) != 0

  final def waitMessage         = new Progress(waitHeader)
  final def strictWaitMessage   = new Progress(waitStrictHeader)
  final def goAhead             = new Progress(goAheadHeader)
  final def unchanged           = new Progress(unchangedHeader)

  @inline final def timeout(millis: Long) = new Progress( timeoutHeader | (millis & valueMask) )
  @inline final def strictTimeout(millis: Long) = new Progress( timeoutStrictHeader | (millis & valueMask) )
  @inline final def sync(k: Int) = new Progress( syncHeader | (k.toLong & valueMask) )

  @inline final def getTimeout(p: Progress): Long = {
    val noMask = (p.value & valueMask)
    (noMask << nMaskBits) >> nMaskBits //to get the sign back
  }

  @inline final def orElse(p1: Progress, p2: Progress): Progress = if (p1 != unchanged) p1 else p2

  //max timeout and strictness
  @inline final def lub(p1: Progress, p2: Progress): Progress = {
    assert(p1 != unchanged && p2 != unchanged)
    val strict = isStrict(p1) || isStrict(p2)
    if (isSync(p1) && isSync(p2)) {
      sync(math.max(p1.k, p2.k))
    } else if (isSync(p1)) {
      p1
    } else if (isSync(p2)) {
      p1
    } else if (isWaitMessage(p1) || isWaitMessage(p2)) {
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
    } else if (isTimeout(p1) && isTimeout(p2)) {
      val to = math.min(getTimeout(p1), getTimeout(p2))
      if (strict) strictTimeout(to) else timeout(to)
    } else if (isTimeout(p1)) {
      if (strict) strictTimeout(getTimeout(p1)) else timeout(getTimeout(p1))
    } else if (isTimeout(p2)) {
      if (strict) strictTimeout(getTimeout(p2)) else timeout(getTimeout(p2))
    } else if (isWaitMessage(p1) && isWaitMessage(p2)) {
      if (strict) strictWaitMessage else waitMessage
    } else if (isWaitMessage(p1)) {
      p1
    } else if (isWaitMessage(p2)) {
      p2
    } else if (isSync(p1) && isSync(p2)) {
      sync(math.min(p1.k, p2.k))
    } else if (isSync(p1)) {
      p1
    } else {
      p2
    }
  }

  final def timeoutInBounds(l: Long) = l == ((l << nMaskBits) >> nMaskBits)

  //TODO
  // default timeout (from config)
  // offset timeout (add/sub a constant from the default timeout)
  // factor timeout (multiply the default timeout)

}
