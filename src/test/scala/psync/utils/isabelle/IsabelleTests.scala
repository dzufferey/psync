package psync.utils.isabelle

import org.scalatest._

class IsabelleTests extends FunSuite {

  //import dzufferey.utils.Logger
  //Logger.moreVerbose
  //Logger.moreVerbose

  test("Session: start, hello, and stop") {
    val s = new Session
    s.start
    val h = s.hello
    println("Isabelle said: " + h)
    s.stop
  }

}
