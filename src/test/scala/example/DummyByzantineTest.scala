package example

import psync._
import psync.runtime._
import psync.macros.Macros._
import psync.utils._
import psync.utils.serialization._

class DBTProcess(timeout: Long) extends Process[Unit] {

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

    }, timeout)
  )
}

class DummyByzantineTest(rt: Runtime, timeout: Long) extends Algorithm[Unit,DBTProcess](rt) {
  
  val spec = TrivialSpec

  def process = new DBTProcess(timeout)

  def dummyIO = ()

}

object DummyByzantineRunner extends Runner {
  
  override def defaultConfFile = "src/test/resources/sample-conf.xml"

  _timeout = 500
  _nbrByzantine = 1

  def onStart {
    val alg = new DummyByzantineTest(rt, timeout)
    Thread.sleep(100)
    Console.println("replica " + id + " starting")
    alg.startInstance(0, ())
  }
  
}
