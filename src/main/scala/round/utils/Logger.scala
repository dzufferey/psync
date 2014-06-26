package round.utils

//logging facility (i.e. syslog alike)

object LogLevel {
    sealed abstract class Level(msg: String, prio: Int, col: String) {
      def message = msg
      def priority = prio
      def color = col
    }
    case object Critical extends Level("Critical", 32, Console.RED)
    case object Error    extends Level("Error",    16, Console.RED)
    case object Warning  extends Level("Warning",  8,  Console.YELLOW)
    case object Notice   extends Level("Notice",   4,  Console.BLUE)
    case object Info     extends Level("Info",     2,  Console.RESET)
    case object Debug    extends Level("Debug",    1,  Console.RESET)
}

//TODO one logger for all or separated logger ? (Console, file, network, ...)

/** Simple logger that outputs to stdout. */
class Logger {

  import LogLevel._

  private var assertive = true
  private var minPriority = Notice.priority
  val disallowed = scala.collection.mutable.HashSet.empty[String]

  def reset = {
    minPriority = Info.priority
    disallowed.clear()
  }
  def getMinPriority = minPriority match {
    case x if x == Critical.priority => Critical
    case x if x == Error.priority =>    Error
    case x if x == Warning.priority =>  Warning
    case x if x == Notice.priority =>   Notice
    case x if x == Info.priority =>     Info
    case x if x == Debug.priority =>    Debug
    case p => sys.error("unknown priority ("+p+")")
  }
  def setMinPriority(lvl: Level) = minPriority = lvl.priority
  def setMinPriority(lvl: Int) = minPriority = lvl
  def disallow(str: String) = disallowed += str
  def allow(str: String) = disallowed -= str

  def disableAssert = assertive = false
  def enableAssert = assertive = true

  private def increaseLevel(l: Level): Level = l match {
    case Critical => Error
    case Error    => Warning
    case Warning  => Notice
    case Notice   => Info
    case Info     => Debug
    case Debug    => Debug
  }
  
  private def decreaseLevel(l: Level): Level = l match {
    case Critical => Critical
    case Error    => Critical
    case Warning  => Error
    case Notice   => Warning
    case Info     => Notice
    case Debug    => Info
  }

  def moreVerbose = setMinPriority( increaseLevel(getMinPriority))

  def lessVerbose = setMinPriority( decreaseLevel(getMinPriority))

  /** Should be dispayed ? */
  def apply(relatedTo: String, lvl: Level): Boolean =
    lvl.priority >= minPriority && !disallowed(relatedTo)

  //The evaluation of the content should *NOT* print. It can cause deadlocks.

  /** Log a message to the console.
   * @param relatedTo The package/file/class from where this message comes from.
   * @param lvl The priority of the message.
   * @param content The content of the message (evaluated only if needed).
   */
  def apply(relatedTo: String, lvl: Level, content: => String): Unit = synchronized {
    if (apply(relatedTo, lvl)) {
      //when content is on multiple lines, each line should be prefixed.
      val prefix = "[" + lvl.color + lvl.message + Console.RESET + "]" + " @ " + relatedTo + ": " 
      val indented = Misc.indent(prefix, content)
      Console.println(indented)
    }
  }
  
  def apply(relatedTo: String, lvl: Level, content: java.io.BufferedWriter => Unit): Unit = synchronized {
    if (apply(relatedTo, lvl)) {
      //when content is on multiple lines, each line should be prefixed.
      val prefix = "[" + lvl.color + lvl.message + Console.RESET + "]" + " @ " + relatedTo + ": " 
      val writer = new java.io.BufferedWriter(new PrefixingWriter(prefix, Console.out))
      content(writer)
      writer.flush
    }
  }

  /** Log a message and throw an exception with the content. */
  def logAndThrow(relatedTo: String, lvl: Level, content: => String): Nothing = {
    apply(relatedTo, lvl, content)
    Console.flush
    sys.error(content)
  }

  def assert(cond: => Boolean, relatedTo: String, content: => String) {
    if (assertive)
      if (!cond)
        logAndThrow(relatedTo, Error, content)
  }

}

object Logger extends Logger {
}
