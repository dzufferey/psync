package example

import round._
import round.runtime._
import round.macros.Macros._

/* A simple example of lattice.
 * Due to the way the serializatino code is generated, we need a concrete type
 * for the algorithm. Unfortunately, we cannot have a version parametric in a 
 * lattice.
 */
object Lattice {
  type T = Set[Int]
  def bottom: T = Set[Int]()
  def join(x: T, xs: T*): T = xs.foldLeft(x)(_ union _)
}

abstract class LatticeIO {
  val initialValue: Lattice.T 
  def decide(value: Lattice.T): Unit
}

class LatticeAgreement extends Algorithm[LatticeIO] {

  import VarHelper._
  import SpecHelper._

  val AD  = new Domain[Lattice.T]

  val active = new LocalVariable[Boolean](true)
  val proposed = new LocalVariable[Lattice.T](Lattice.bottom)
  val decision = new LocalVariable[Option[Lattice.T]](None) //TODO as ghost
  //
  val callback = new LocalVariable[LatticeIO](null)

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
  
  def process = p(new Process[LatticeIO] {

    def init(io: LatticeIO) {
      callback <~ io
      active <~ true
      proposed <~ io.initialValue
    }
    
    val rounds = Array[Round](
      rnd(new Round {
        
        type A = Lattice.T

        def send(): Set[(Lattice.T, ProcessID)] = {
          broadcast(proposed)
        }

        def update(mailbox: Set[(Lattice.T, ProcessID)]) {
          if (active) {
            if (mailbox.filter(_._1 == (proposed: Lattice.T)).size > n/2) {
              callback.decide(proposed)
              decision <~ Some(proposed)
              active <~ false
              terminate()
            } else {
              proposed <~ Lattice.join(proposed, mailbox.map(_._1).toSeq:_*)
            }
          }
        }

      })
    )

  })

}

object LatticeRunner extends RTOptions {
  
  var confFile = "src/test/resources/3replicas-conf.xml"
  
  val usage = "..."
  
  var rt: RunTime[LatticeIO] = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[java.lang.String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    val alg = new LatticeAgreement()
    rt = new RunTime(alg, this, defaultHandler(_))
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
