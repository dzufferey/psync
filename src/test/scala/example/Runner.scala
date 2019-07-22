package example

import psync._
import psync.runtime._

abstract class Runner extends RTOptions {
  
  val usage = "..."
  
  var rt: Runtime = null
  
  def defaultConfFile: String = "src/test/resources/3replicas-conf.xml"

  def defaultHandler(msg: Message) {
    msg.release
  }

  def onStart: Unit

  def onShutdown {
    if (rt != null) {
      rt.shutdown
    }
  }
  
  def main(args: Array[java.lang.String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: defaultConfFile +: args
    apply(args2)
    rt = Runtime(this, defaultHandler(_))
    rt.startService
    onStart
  }
  
  java.lang.Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        rt.shutdown
      }
    }
  )
}
