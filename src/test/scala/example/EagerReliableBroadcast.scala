package example

import psync._
import psync.runtime._
import psync.macros.Macros._
import psync.utils.serialization._

abstract class BroadcastIO {
  val initialValue: Option[Int]
  def deliver(value: Int): Unit
}

class ErbProcess(timeout: Long) extends Process[BroadcastIO] {
  
  var x: Option[Int] = None
  var callback: BroadcastIO = null
    
  def init(io: BroadcastIO) = i{
    callback = io
    x = io.initialValue
  }

  val rounds = phase(
    new Round[Int](timeout){
    
      def send: Map[ProcessID,Int] = {
        if (x.isDefined) broadcast(x.get)
        else Map.empty[ProcessID,Int]
      }

      def update(mailbox: Map[ProcessID,Int]): Unit = {
        if (x.isDefined) {
          callback.deliver(x.get)
          exitAtEndOfRound
        } else {
          if (!mailbox.isEmpty) {
            x = Some(mailbox.head._2)
          } else if (r > 10) { //crash before delivering
            exitAtEndOfRound
          }
        }
      }

    }
  )
  
}

//http://link.springer.com/chapter/10.1007%2F978-3-642-15260-3_3
class EagerReliableBroadcast(rt: Runtime, timeout: Long) extends Algorithm[BroadcastIO,ErbProcess](rt) {

  val spec = TrivialSpec

  def process = new ErbProcess(timeout)

  def dummyIO = new BroadcastIO{
    val initialValue = None
    def deliver(value: Int): Unit = { }
  }
}

object ERBRunner extends Runner {
  
  var alg: EagerReliableBroadcast = null

  val delivered = new java.util.concurrent.ConcurrentHashMap[Short, Boolean]

  def other = new BroadcastIO {
    val initialValue = None
    def deliver(v: Int): Unit = {
      Console.println(s"$id delivering $v")
    }
  }

  override def defaultHandler(msg: Message): Unit = {
    val inst = msg.tag.instanceNbr
    val already = delivered.putIfAbsent(inst, true)
    if (already) {
      msg.release
    } else {
      alg.startInstance(inst, other, Set(msg))
    }
  }

  def onStart: Unit = {
    alg = new EagerReliableBroadcast(rt, timeout)

    import scala.util.Random
    val init = Random.nextInt
    val io = new BroadcastIO {
      val initialValue = Some(init) 
      def deliver(v: Int): Unit = {
        Console.println(s"$id delivering $v")
      }
    }
    Thread.sleep(100)
    Console.println(s"replica $id proposing $init")
    delivered.put(id, true)
    alg.startInstance(id, io)
  }
  
}
