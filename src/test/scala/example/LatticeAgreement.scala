package example

import psync._
import psync.runtime._
import psync.macros.Macros._
import psync.utils.serialization._

/* A simple example of lattice.
 * Due to the way the serializatino code is generated, we need a concrete type
 * for the algorithm. Unfortunately, we cannot have a version parametric in a 
 * lattice.
 */
object Lattice {
  type T = Set[Int]
  def bottom: T = Set[Int]()
  def join(x: T, xs: T*): T = xs.foldLeft(x)(_ union _)
  implicit val regLatticeT = new KryoRegistration[T] {
    val setSerializer = new CollectionSerializer[Int, Set[Int]]
    override def registerClassesWithSerializer = Seq(
      classOf[T] -> setSerializer
    )
  }
}

import Lattice.regLatticeT

abstract class LatticeIO {
  val initialValue: Lattice.T 
  def decide(value: Lattice.T): Unit
}

class LatticeAgreementProcess extends Process[LatticeIO]{
  
  var active = true
  var proposed = Lattice.bottom
  var decision: Option[Lattice.T] = None //TODO as ghost
  var callback: LatticeIO = null
    
  def init(io: LatticeIO) = i{
    callback = io
    active = true
    proposed = io.initialValue
  }

  val rounds = phase(
    new Round[Lattice.T] {
      
      def send(): Map[ProcessID,Lattice.T] = {
        broadcast(proposed)
      }

      def update(mailbox: Map[ProcessID,Lattice.T]) {
        if (active) {
          if (mailbox.count{ case (k,v) => v == proposed} > n/2) {
            callback.decide(proposed)
            decision = Some(proposed)
            active = false
            exitAtEndOfRound()
          } else {
            proposed = Lattice.join(proposed, mailbox.map(_._2).toSeq:_*)
          }
        }
      }

    }
  )

}

class LatticeAgreement extends Algorithm[LatticeIO,LatticeAgreementProcess] {

  val AD  = new Domain[Lattice.T]

  val spec = TrivialSpec //TODO

  //here we should provide some axioms that the prover will need ...
  Axiom("join-idempotent", f( AD.forall( x => Lattice.join(x, x) == x) ))
  Axiom("join-bottom", f( AD.forall( x => Lattice.join(Lattice.bottom, x) == x) ))
  Axiom("join-commute", f( AD.forall( x => AD.forall( y =>  Lattice.join(x, y) == Lattice.join(y, x) )) ))
  Axiom("join-assoc", f( AD.forall( x => AD.forall( y => AD.forall( z => Lattice.join(Lattice.join(x, y), z) == Lattice.join(x, Lattice.join(y, z)) ))) ))
  //TODO distributivity,
  //TODO generalize to polyadic version
  Axiom("join-singleton", f( AD.forall( x => Lattice.join(x) == x) ))
  //TODO can we have a local axiomatization of join ? after all there is some idempotence property.
  
  def process = new LatticeAgreementProcess

  def dummyIO = new LatticeIO{
    val initialValue = Lattice.bottom
    def decide(value: Lattice.T) { }
  }
}

object LatticeRunner extends RTOptions {
  
  var confFile = "src/test/resources/3replicas-conf.xml"
  
  val usage = "..."
  
  var rt: Runtime[LatticeIO,LatticeAgreementProcess] = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[java.lang.String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    val alg = new LatticeAgreement()
    rt = new Runtime(alg, this, defaultHandler(_))
    rt.startService

    import scala.util.Random
    val n = Random.nextInt(5) + 1
    val init = (0 until n).map(_ => Random.nextInt).toSet
    val io = new LatticeIO {
      val initialValue = init
      def decide(value: Lattice.T) {
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
