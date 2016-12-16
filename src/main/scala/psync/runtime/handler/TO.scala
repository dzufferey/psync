package psync.runtime.handler

class TO(val nextTimeout: Long) extends AnyVal {

  def isTerminated = nextTimeout < 0l

  def isNextTimeout = nextTimeout > 0l

  def isNothing = nextTimeout == 0l

}
