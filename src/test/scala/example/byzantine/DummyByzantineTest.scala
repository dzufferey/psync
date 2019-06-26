package example.byzantine

import psync._
import example._
import psync.runtime._
import psync.macros.Macros._
import psync.utils._
import psync.utils.serialization._

class DBTProcess(timeout: Long, wait: Boolean) extends Process[Unit] {

  def init(io: Unit) { }

  val rounds = phase(
    new PessimisticByzantineSynchronizer(new EventRound[Unit]{
    
      def init = {
        Progress.timeout( timeout )
      }

      def send: Map[ProcessID,Unit] = {
        val neighbor = if (id.id % 2 == 0) new ProcessID(( (id.id + 1) % n).toShort)
                       else new ProcessID((id.id - 1).toShort)
        Map((neighbor, ()))
      }

      def receive(sender: ProcessID, v: Unit) = {
        Console.println(id + " got a message from " + sender + " at round " + r.toInt)
        Progress.goAhead
      }

    }, timeout, wait)
  )
}

class DummyByzantineTest(rt: Runtime, timeout: Long, _wait: Boolean) extends Algorithm[Unit,DBTProcess](rt) {
  
  val spec = TrivialSpec

  def process = new DBTProcess(timeout, _wait)

  def dummyIO = ()

}

object DummyByzantineRunner extends Runner {
  
  override def defaultConfFile = "src/test/resources/sample-conf.xml"

  _nbrByzantine = 1
  _delayFirstSend = 3000
  var _wait = true
  newOption("--noWait", dzufferey.arg.Unit( () => _wait = false), "always TO")

  def onStart {
    val alg = new DummyByzantineTest(rt, timeout, _wait)
    Thread.sleep(100)
    Console.println("replica " + id + " starting")
    alg.startInstance(0, ())
  }
  
}
