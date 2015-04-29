package example

import round._
import round.runtime._
import round.macros.Macros._


abstract class TpcIO {
  val coord: Short
  val canCommit: Boolean
  def decide(value: Option[Boolean]): Unit //deciding None means that we suspect the coordinator of crash!
}

class TwoPhaseCommit extends Algorithm[TpcIO] {

  import VarHelper._
  import SpecHelper._

  //cannot have ProcessID:  Result type in structural refinement may not refer to a user-defined value class
  //TODO look into this, probably abug in the compiler
  val coord = new LocalVariable[Short](0)
  val vote = new LocalVariable[Boolean](false)
  val decision = new LocalVariable[Option[Boolean]](None) //TODO as ghost
  //
  val callback = new LocalVariable[TpcIO](null)

  def c(pid: Short) = new ProcessID(pid)

  Axiom("well-coordinated", f( P.exists( p => P.forall( q => (p: ProcessID) == c(coord(q))) )))

  val spec = new Spec {
    val livenessPredicate = List(
      f(P.exists( p => P.forall( q => (p: ProcessID) == c(coord(q)) && HO(p).size == n && (HO(q) contains p) ) ))
    )
    val invariants = List(
      f( P.forall( p => decision(p) == Some(true) ==> P.forall( q => vote(q) )) && 
         P.forall( p => P.forall( q => (decision(p).isDefined && decision(q).isDefined) ==> (decision(p) == decision(q)) ))
      )
    )
//  override val roundInvariants = List(
//    List(f(true)),
//    List(f(true))
//  )
    val properties = List(
      "Uniform Agreement" -> f( P.forall( p => P.forall( q => (decision(p).isDefined && decision(q).isDefined) ==> (decision(p) == decision(q)) ))),
      //"Irrevocable" -> A site cannot reverse its decision after it has reached one. TODO need better handling of termination
      "Validity" -> f( P.forall( p => (decision(p) == Some(true)) ==> P.forall( q => vote(q) ) ))
      //"Non-Triviality" -> If there are no fault and all sites voted Yes, then the decision will be to commit. TODO need to reason about HO in the whole run
      //"Termination" -> At any point in the execution of the protocol, if all existing failures are repaired and no new failures occur for sufficiently long, then all sites will eventually reach a decision. TODO we have a different fault model
    )
  }

  def process = p(new Process[TpcIO]{

    def init(io: TpcIO) {
      callback <~ io
      coord <~ io.coord
      vote <~ io.canCommit
      decision <~ None
    }

    val rounds = Array[Round](
      rnd(new Round{
        type A = Boolean //place holder for PrepareCommit
        def send(): Set[(Boolean, ProcessID)] = {
          if (id == c(coord)) broadcast(true)
          else Set.empty
        }
        override def expectedNbrMessages = 1
        def update(mailbox: Set[(Boolean, ProcessID)]) {
          //nothing to do
        } 
      }),

      rnd(new Round{
        type A = Boolean

        def send(): Set[(Boolean, ProcessID)] = {
          Set( (vote: Boolean) -> c(coord) )
        }

        def update(mailbox: Set[(Boolean, ProcessID)]) {
          if (id == c(coord)) {
            if( mailbox.size == (n: Int) &&
                mailbox.forall( _._1 ))
            {
              decision <~ Some(true)
            } else {
              decision <~ Some(false)
            }
          }
        }
      }),

      rnd(new Round{
        type A = Boolean

        def send(): Set[(Boolean, ProcessID)] = {
          if (id == c(coord)) broadcast((decision: Option[Boolean]).get)
          else Set.empty
        }

        override def expectedNbrMessages = 1
        def update(mailbox: Set[(Boolean, ProcessID)]) {
          if (mailbox.size > 0) {
            decision <~ Some(mailbox.head._1)
          }
          callback.decide(decision)
          terminate
        }
      })
    )
  })

}


object TpcRunner extends RTOptions {
  

  var confFile = "src/test/resources/sample-conf.xml"
  
  val usage = "..."
  
  var rt: RunTime[TpcIO] = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    val alg = new TwoPhaseCommit()
    rt = new RunTime(alg, this, defaultHandler(_))
    rt.startService

    import scala.util.Random
    val init = Random.nextBoolean
    val io = new TpcIO {
      val coord: Short = 0
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
