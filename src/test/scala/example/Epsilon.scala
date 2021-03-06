package example

import psync._
import psync.Time._
import psync.runtime._
import psync.macros.Macros._
import psync.utils.serialization._
import Ordering.Double.TotalOrdering

abstract class RealConsensusIO {
  val initialValue: Double
  def decide(value: Double): Unit
}

//http://www.cambridge.org/us/download_file/175769/
class EpsilonProcess(f: Int, epsilon: Double, timeout: Long) extends Process[RealConsensusIO] {

  var x = 0.0
  var maxR = new Time(0)
  var halted = Map[ProcessID, Double]()
  var callback: RealConsensusIO = null

  def init(io: RealConsensusIO) = i{
    callback = io
    x = io.initialValue
  }

  val rounds = phase(

    new Round[(Double,Boolean)](timeout){
     
      def diff(s: Iterable[Double]) = s.max - s.min //this is never defined in the slides. double check that this is the right thing...
      def c(m: Int, k: Int) = (m-1) / k + 1
      def reduce(f: Int, collection: Seq[Double]) = {
        collection.sorted.drop(f).dropRight(f)
      }
      def select(k: Int, collection: Seq[Double]) = {
        collection.grouped(k).map(_.head).toSeq
      }
      def _new(k: Int, f: Int, collection: Seq[Double]) = {
        val red = reduce(f, collection)
        val sel = select(k, red)
        sel.sum / sel.size
      }
     
      def send: Map[ProcessID,(Double, Boolean)] = {
        if (r <= maxR) {
          broadcast( x -> false )
        } else {
          broadcast( x -> true )
        }
      }
     
      def update(mailbox: Map[ProcessID,(Double, Boolean)]): Unit = {
        val V = mailbox.toSeq.map(_._2._1) ++ halted.values
        halted = halted ++ mailbox.filter(_._2._2).map{ case (p,v) => p -> v._1 }
        if (r.toInt == 0) {
          //assert(mailbox.size >= n - f, mailbox.size)
          val r1 = math.log(diff(V) / epsilon) / math.log(c(n-3*f, 2*f))
          maxR = math.ceil(r1).toInt
          x = reduce(2*f, V).head
        } else if (r <= maxR) {
          x = _new(2*f, f, V)
        } else {
          callback.decide(x)
          exitAtEndOfRound
        }
      }
    }
  )
}

class EpsilonConsensus(rt: Runtime, f: Int, epsilon: Double, timeout: Long) extends Algorithm[RealConsensusIO,EpsilonProcess](rt) {

  val spec = TrivialSpec //TODO we need to add support for Real numbers

  def process = new EpsilonProcess(f, epsilon, timeout: Long)

  def dummyIO = new RealConsensusIO{
    val initialValue = 0.0
    def decide(value: Double): Unit = { }
  }
}

object EpsilonRunner extends Runner {
  
  var f = 1
  newOption("-f", dzufferey.arg.Int( i => f = i), "f (default = 1)")
  
  var e = 0.1
  //newOption("-e", dzufferey.arg.Real( i => e = i), "ε (default = 0.1)")

  override def defaultConfFile = "src/test/resources/7replicas-conf.xml"
  
  def onStart: Unit = {
    val start = java.lang.System.currentTimeMillis()
    val alg = new EpsilonConsensus(rt, f, e, timeout)

    import scala.util.Random
    val init = Random.nextDouble
    val io = new RealConsensusIO {
      val initialValue = init
      def decide(value: Double): Unit = {
        Console.println("replica " + id + " decided " + value)
      }
    }
    val cur = java.lang.System.currentTimeMillis()
    Thread.sleep(5000 + start - cur)
    Console.println("replica " + id + " starting with " + init)
    alg.startInstance(0, io)
  }
  
}
