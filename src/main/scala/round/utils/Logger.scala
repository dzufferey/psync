package round.utils

//logging facility (i.e. syslog alike)

sealed abstract class LogLevel(msg: String, prio: Int, col: String) {
  def message = msg
  def priority = prio
  def color = col
}
case object LogCritical extends LogLevel("Critical", 32, Console.RED)
case object LogError    extends LogLevel("Error",    16, Console.RED)
case object LogWarning  extends LogLevel("Warning",  8,  Console.YELLOW)
case object LogNotice   extends LogLevel("Notice",   4,  Console.BLUE)
case object LogInfo     extends LogLevel("Info",     2,  Console.RESET)
case object LogDebug    extends LogLevel("Debug",    1,  Console.RESET)

//TODO one logger for all or separated logger ? (Console, file, network, ...)

/** Simple logger that outputs to stdout. */
class Logger {

  private var assertive = true
  private var minPriority = LogNotice.priority
  val disallowed = scala.collection.mutable.HashSet.empty[String]

  def reset = {
    minPriority = LogInfo.priority
    disallowed.clear()
  }
  def getMinPriority = minPriority match {
    case x if x == LogCritical.priority => LogCritical
    case x if x == LogError.priority => LogError
    case x if x == LogWarning.priority => LogWarning
    case x if x == LogNotice.priority => LogNotice
    case x if x == LogInfo.priority => LogInfo
    case x if x == LogDebug.priority => LogDebug
    case p => sys.error("unknown priority ("+p+")")
  }
  def setMinPriority(lvl: LogLevel) = minPriority = lvl.priority
  def setMinPriority(lvl: Int) = minPriority = lvl
  def disallow(str: String) = disallowed += str
  def allow(str: String) = disallowed -= str

  def disableAssert = assertive = false
  def enableAssert = assertive = true

  private def increaseLevel(l: LogLevel): LogLevel = l match {
    case LogCritical => LogError
    case LogError    => LogWarning
    case LogWarning  => LogNotice
    case LogNotice   => LogInfo
    case LogInfo     => LogDebug
    case LogDebug    => LogDebug
  }
  
  private def decreaseLevel(l: LogLevel): LogLevel = l match {
    case LogCritical => LogCritical
    case LogError    => LogCritical
    case LogWarning  => LogError
    case LogNotice   => LogWarning
    case LogInfo     => LogNotice
    case LogDebug    => LogInfo
  }

  def moreVerbose = setMinPriority( increaseLevel(getMinPriority))

  def lessVerbose = setMinPriority( decreaseLevel(getMinPriority))

  /** Should be dispayed ? */
  def apply(relatedTo: String, lvl: LogLevel): Boolean =
    lvl.priority >= minPriority && !disallowed(relatedTo)

  //The evaluation of the content should *NOT* print. It can cause deadlocks.

  /** Log a message to the console.
   * @param relatedTo The package/file/class from where this message comes from.
   * @param lvl The priority of the message.
   * @param content The content of the message (evaluated only if needed).
   */
  def apply(relatedTo: String, lvl: LogLevel, content: => String): Unit = synchronized {
    if (apply(relatedTo, lvl)) {
      //when content is on multiple lines, each line should be prefixed.
      val prefix = "[" + lvl.color + lvl.message + Console.RESET + "]" + " @ " + relatedTo + ": " 
      val indented = Misc.indent(prefix, content)
      Console.println(indented)
    }
  }
  
  def apply(relatedTo: String, lvl: LogLevel, content: java.io.BufferedWriter => Unit): Unit = synchronized {
    if (apply(relatedTo, lvl)) {
      //when content is on multiple lines, each line should be prefixed.
      val prefix = "[" + lvl.color + lvl.message + Console.RESET + "]" + " @ " + relatedTo + ": " 
      val writer = new java.io.BufferedWriter(new PrefixingWriter(prefix, Console.out))
      content(writer)
      writer.flush
    }
  }

  /** Log a message and throw an exception with the content. */
  def logAndThrow(relatedTo: String, lvl: LogLevel, content: => String): Nothing = {
    apply(relatedTo, lvl, content)
    Console.flush
    sys.error(content)
  }

  def assert(cond: => Boolean, relatedTo: String, content: => String) {
    if (assertive)
      if (!cond)
        logAndThrow(relatedTo, LogError, content)
  }

}

object Logger extends Logger {
}
