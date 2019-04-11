package example

import psync._
import psync.runtime._
import psync.macros.Macros._
import psync.utils.serialization._
import scala.reflect.ClassTag

// https://ti.tuwien.ac.at/ecs/people/schmid/Mypapers/WS09.pdf
// Θ is a (possibly unknown) bound on the ratio of longest / shortest end-to-end communication delays.
// Two versions:
// * page 5: we need Ξ ≥ 3 Θ
// * page 7:
// for both algorithms we use the round number as `k`

abstract class TmIO[A]() {
  def getMessage(t: Time, pid: ProcessID): A
  def deliverMessage(t: Time, pid: ProcessID, a: A): Unit
}

object TmOrSerialization {
  implicit def regOpt[A: ClassTag] = new KryoRegistration[Option[A]] {
    import scala.language.existentials // for None
    val optionSerializer = new OptionSerializer[A]
    override def registerClassesWithSerializer = Seq(
      classOf[Option[A]] -> optionSerializer,
      classOf[Some[A]] -> optionSerializer,
      None.getClass -> optionSerializer
    )
  }
}

import TmOrSerialization._

class TmProcess[A: ClassTag: KryoRegistration](f: Int, theta: Double) extends Process[TmIO[A]] {
  
  var callback: TmIO[A]= null
  var round = new Time(0)
  var nextRoundAt = new Time(0)

  def updateNextRoundAt {
    if (theta >= 1) {
      nextRoundAt = new Time((3 * theta * (round + 1)).toInt + 1)
    } else { //unkown theta
      nextRoundAt = new Time((round + 1) * (round + 2) / 2)
    }
    //Console.println("Replica " + id.id + ": r = " + r.toInt + ", round = " + round.toInt + ", nextRoundAt = " + nextRoundAt.toInt)
  }

  def init(io: TmIO[A]) = i{
    callback = io
    round = new Time(0)
    updateNextRoundAt
  }
  
  val rounds = phase(
    new EventRound[Option[A]]{

      var needToSend = false
      var received = 0

      def init = {
        if (r == nextRoundAt) {
          needToSend = true
        } else {
          needToSend = false
        }
        received = 0
        Progress.waitMessage
      }
    
      def send: Map[ProcessID,Option[A]] = {
        if (needToSend) {
          (0 until n).foldLeft(Map[ProcessID,Option[A]]())( (acc, i) => {
            val pid = new ProcessID(i.toShort)
            acc + (pid -> Some(callback.getMessage(round, pid)))
          })
        } else {
          broadcast(None)
        }
      }

      def receive(sender: ProcessID, payload: Option[A]) = {
        received += 1
        payload.foreach( a => callback.deliverMessage(round, sender, a) )
        if (received >= n - f) Progress.goAhead
        else Progress.waitMessage
      }

      override def finishRound(didTimeout: Boolean) = {
        if (needToSend) {
          round = round.tick
          updateNextRoundAt
        }
        //Thread.sleep(100)
        true
      }
    }
  )

}

class ThetaModel(rt: Runtime, f: Int, theta: Double) extends Algorithm[TmIO[String],TmProcess[String]](rt) {
  
  val spec = TrivialSpec

  def process = new TmProcess(f, theta)

  def dummyIO = new TmIO[String]{
      def getMessage(t: Time, pid: ProcessID) = ""
      def deliverMessage(t: Time, pid: ProcessID, str: String) { }
  }
}

object TmRunner extends RTOptions {

  var f = 1
  newOption("-f", dzufferey.arg.Int( i => f = i), "(default = 1)")
  var theta = 2.0
  newOption("--theta", dzufferey.arg.Real( i => theta = i), "(default = 2.0)")

  var confFile = "src/test/resources/sample-conf.xml"
  
  val usage = "..."
  
  var rt: Runtime = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[java.lang.String]) {
    val start = java.lang.System.currentTimeMillis()
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    rt = new Runtime(this, defaultHandler(_))
    rt.startService
    val alg = new ThetaModel(rt, f, theta)

    val io = new TmIO[String]{
      def getMessage(t: Time, pid: ProcessID) = "Hello " + pid.id
      def deliverMessage(t: Time, pid: ProcessID, str: String) {
        Console.println(pid.id + " says \"" + str + "\"")
      }
    }

    val cur = java.lang.System.currentTimeMillis()
    Thread.sleep(3000 + start - cur) //time to run all the processes
    alg.startInstance(0, io)
    // idle while the algorithms runs
    while (true) Thread.sleep(100000)
  }
  
  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        rt.shutdown
      }
    }
  )

}
