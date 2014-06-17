package round.utils

import scala.sys.process._

/** executing command as children process */
object SysCmd {

  private val limiter: java.util.concurrent.Semaphore = {
    //if (Config.maxChildren >= 0)
    //  new java.util.concurrent.Semaphore(Config.maxChildren, true)
    //else
      null
  }

  def acquire {
    if (limiter != null) limiter.acquire
  }
  
  def release {
    if (limiter != null) limiter.release
  }

  type ExecResult = (Int, String, String)
  
  //TODO add an option for timeout
  def apply(cmds: Array[String], input: Option[String], addToEnv: (String,String)*): ExecResult = {
    val process = Process(cmds, None, addToEnv:_*)
    val withInput = input match {
      case Some(str) => process #< ( new java.io.ByteArrayInputStream(str.getBytes) )
      case None => process
    }

    val bufferOut = new StringBuilder()
    val bufferErr = new StringBuilder()
    val processLogger =
      ProcessLogger(
        line => {bufferOut append line; bufferOut append "\n"},
        line => {bufferErr append line; bufferErr append "\n"}
      )
    Logger("Utils", LogInfo, "Executing "+ cmds.mkString(""," ",""))
    try {
      acquire
      val exitCode = withInput ! processLogger
      (exitCode, bufferOut.toString, bufferErr.toString)
    } finally {
      release
    }
  }
  
  def apply(cmds: Array[String], input: String, addToEnv: (String,String)*): ExecResult =
    apply(cmds, Some(input), addToEnv: _*)

  def apply(cmds: Array[String], addToEnv: (String,String)*): ExecResult =
    apply(cmds, None, addToEnv: _*)
  
  
  def execWithoutOutput(cmds: Array[String], input: Option[String], addToEnv: (String,String)*): Int = {
    val process = Process(cmds, None, addToEnv:_*)
    val withInput = input match {
      case Some(str) => process #< ( new java.io.ByteArrayInputStream(str.getBytes) )
      case None => process
    }
    Logger("Utils", LogInfo, "Executing "+ cmds.mkString(""," ",""))
    try {
      acquire
      withInput.! 
    } finally {
      release
    }
  }
  
  def execRedirectToLogger(cmds: Array[String], input: Option[String], prefix: String, lvl: LogLevel, addToEnv: (String,String)*): Int = {
    val process = Process(cmds, None, addToEnv:_*)
    val withInput = input match {
      case Some(str) => process #< ( new java.io.ByteArrayInputStream(str.getBytes) )
      case None => process
    }
    val processLogger = ProcessLogger(
      out => Logger(prefix, lvl, out),
      err => Logger(prefix, LogWarning, err))
    Logger("Utils", LogInfo, "Executing "+ cmds.mkString(""," ",""))
    try {
      acquire
      withInput ! processLogger
    } finally {
      release
    }
  }
  
  def execOutputAndLog(cmds: Array[String], input: Option[String], prefix: String, lvl: LogLevel, addToEnv: (String,String)*): ExecResult = {
    val process = Process(cmds, None, addToEnv:_*)
    val withInput = input match {
      case Some(str) => process #< ( new java.io.ByteArrayInputStream(str.getBytes) )
      case None => process
    }

    val bufferOut = new StringBuilder()
    val bufferErr = new StringBuilder()
    val processLogger =
      ProcessLogger(
        line => {Logger(prefix, lvl, line); bufferOut append line; bufferOut append "\n"},
        line => {Logger(prefix, LogWarning, line); bufferErr append line; bufferErr append "\n"}
      )
    Logger("Utils", LogInfo, "Executing "+ cmds.mkString(""," ",""))
    try {
      acquire
      val exitCode = withInput ! processLogger
      (exitCode, bufferOut.toString, bufferErr.toString)
    } finally {
      release
    }
  }

}
