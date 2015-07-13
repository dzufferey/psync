package example

import round._
import round.runtime._
import round.macros.Macros._

abstract class BroadcastIO {
  val initialValue: Option[Int]
  def deliver(value: Int): Unit
}

//http://link.springer.com/chapter/10.1007%2F978-3-642-15260-3_3
class EagerReliableBroadcast extends Algorithm[BroadcastIO] {

  import VarHelper._
  import SpecHelper._

  val x = new LocalVariable[Option[Int]](None)
  val callback = new LocalVariable[BroadcastIO](null)
  
  val spec = TrivialSpec

  def process = p(new Process[BroadcastIO]{
      
    def init(io: BroadcastIO) {
      callback <~ io
      x <~ io.initialValue
    }

    val rounds = phase(
      new Round{
      
        type A = Int

        def send: Set[(Int,ProcessID)] = {
          if (x.isDefined) broadcast(x.get) else Set.empty
        }

        def update(mailbox: Set[(Int, ProcessID)]) {
          if (x.isDefined) {
            callback.deliver(x.get)
            terminate
          } else {
            if (!mailbox.isEmpty) {
              x <~ Some(mailbox.head._1)
            } else if (r > 10) { //crash before delivering
              terminate
            }
          }
        }

      }
    )
  })

}

object ERBRunner extends RTOptions {
  
  var confFile = "src/test/resources/3replicas-conf.xml"
  
  val usage = "..."
  
  var rt: RunTime[BroadcastIO] = null

  val delivered = new java.util.concurrent.ConcurrentHashMap[Short, Boolean]

  def other = new BroadcastIO {
    val initialValue = None
    def deliver(v: Int) = {
      Console.println(id + " delivering " + v)
    }
  }

  def defaultHandler(msg: Message) {
    val inst = msg.tag.instanceNbr
    val already = delivered.putIfAbsent(inst, true)
    if (already) {
      msg.release
    } else {
      rt.startInstance(inst, other, Set(msg))
    }
  }
  
  def main(args: Array[java.lang.String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    val alg = new EagerReliableBroadcast
    rt = new RunTime(alg, this, defaultHandler(_))
    rt.startService

    import scala.util.Random
    val init = Random.nextInt
    val io = new BroadcastIO {
      val initialValue = Some(init) 
      def deliver(v: Int) {
        Console.println(id + " delivering " + v)
      }
    }
    Thread.sleep(100)
    Console.println("replica " + id + " proposing " + init)
    delivered.put(id.toShort, true)
    rt.startInstance(id.toShort, io)
  }
  
  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        rt.shutdown
      }
    }
  )
}
