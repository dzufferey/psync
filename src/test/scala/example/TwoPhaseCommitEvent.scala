package example

import psync._
import psync.formula._
import psync.runtime._
import psync.macros.Macros._


class TpcEvtProcess(blocking: Boolean, timeout: Long) extends Process[TpcIO] {
  
  var coord = new ProcessID(0)
  var vote = false
  var decision: Option[Boolean] = None //TODO as ghost
  var callback: TpcIO = null

  def init(io: TpcIO) = i{
    callback = io
    coord = io.coord
    vote = io.canCommit
    decision = None
  }
    
  val rounds = phase(
    new EventRound[Boolean]{ //place holder for PrepareCommit

      def init = {
        if (blocking) Progress.waitMessage
        else Progress.timeout(timeout)
      }

      def send(): Map[ProcessID,Boolean] = {
        if (id == coord) broadcast(true)
        else Map.empty[ProcessID,Boolean] //otherwise the compiler give Map[ProcessID,Int] !?
      }

      def receive(sender: ProcessID, payload: Boolean) = {
        Progress.goAhead
      }

    },

    new EventRound[Boolean]{

      var nMsg = 0

      def init = {
        nMsg = 0
        if (id != coord) Progress.goAhead
        else if (blocking) Progress.waitMessage
        else Progress.timeout(timeout)
      }

      def send(): Map[ProcessID,Boolean] = {
        Map( coord -> vote )
      }

      def receive(sender: ProcessID, payload: Boolean) = {
        nMsg += 1
        if (!payload || nMsg == n) Progress.goAhead
        else Progress.unchanged
      }

      override def finishRound(didTimeout: Boolean) = {
        if (id == coord) {
          decision = Some(nMsg == n)
        }
        true
      }

    },

    new EventRound[Boolean]{

      def init = {
        if (blocking) Progress.waitMessage
        else Progress.timeout(timeout)
      }

      def send(): Map[ProcessID,Boolean] = {
        if (id == coord) broadcast(decision.get)
        else Map.empty[ProcessID,Boolean]
      }

      def receive(sender: ProcessID, payload: Boolean) = {
        decision = Some(payload)
        Progress.goAhead
      }

      override def finishRound(didTimeout: Boolean) = {
        callback.decide(decision)
        false
      }

    }
  )

}

class TwoPhaseCommitEvent(blocking: Boolean, timeout: Long) extends Algorithm[TpcIO,TpcEvtProcess] {

  import SpecHelper._

  val spec = TrivialSpec //TODO

  def process = new TpcEvtProcess(blocking, timeout)

  def dummyIO = new TpcIO{
    val coord = new ProcessID(0)
    val canCommit = false
    def decide(value: Option[Boolean]) { }
  }
}


object TpcEvtRunner extends RTOptions {
  
  var confFile = "src/test/resources/sample-conf.xml"
  
  var blocking = false
  newOption("-blocking", dzufferey.arg.Unit( () => blocking = true), "waitMessage for all messages (no timeout)")

  val usage = "..."
  
  var rt: Runtime[TpcIO,TpcEvtProcess] = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    //Console.println("starting " + id + " with blocking = " + blocking + ", timeout = " + timeout)
    val alg = new TwoPhaseCommitEvent(blocking, timeout)
    rt = new Runtime(alg, this, defaultHandler(_))
    rt.startService

    import scala.util.Random
    val init = Random.nextBoolean
    val io = new TpcIO {
      val coord = new ProcessID(0)
      val canCommit = init
      val initialValue = init
      def decide(value: Option[Boolean]) {
        Console.println("replica " + id + " decided " + value)
      }
    }
    Thread.sleep(100)
    Console.println("replica " + id + " starting with " + init)
    rt.startInstance(0, io)
  }
  
  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        rt.shutdown
      }
    }
  )
}
