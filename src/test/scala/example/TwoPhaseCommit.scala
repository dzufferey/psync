package example

import psync._
import psync.formula._
import psync.runtime._
import psync.macros.Macros._


abstract class TpcIO {
  val coord: ProcessID
  val canCommit: Boolean
  def decide(value: Option[Boolean]): Unit //deciding None means that we suspect the coordinator of crash!
}

class TpcProcess(timeout: Long) extends Process[TpcIO] {
  
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
    new Round[Boolean](timeout){ //place holder for PrepareCommit
      def send(): Map[ProcessID,Boolean] = {
        if (id == coord) broadcast(true)
        else Map.empty[ProcessID,Boolean] //otherwise the compiler give Map[ProcessID,Int] !?
      }
      override def expectedNbrMessages = 1
      def update(mailbox: Map[ProcessID,Boolean]) {
        //nothing to do
      } 
    },

    new Round[Boolean](timeout){

      def send(): Map[ProcessID,Boolean] = {
        Map( coord -> vote )
      }
      
      override def expectedNbrMessages = if (id == coord) n else 0

      def update(mailbox: Map[ProcessID,Boolean]) {
        if (id == coord) {
          if( mailbox.size == n && mailbox.forall{ case (k,v) => v }) {
            decision = Some(true)
          } else {
            decision = Some(false)
          }
        }
      }
    },

    new Round[Boolean](timeout){

      def send(): Map[ProcessID,Boolean] = {
        if (id == coord) broadcast(decision.get)
        else Map.empty[ProcessID,Boolean] //otherwise the compiler give Map[ProcessID,Int] !?
      }

      override def expectedNbrMessages = 1

      def update(mailbox: Map[ProcessID,Boolean]) {
        if (mailbox.size > 0) {
          decision = Some(mailbox.head._2)
        }
        callback.decide(decision)
        exitAtEndOfRound()
      }
    }
  )

}

class TwoPhaseCommit(timeout: Long) extends Algorithm[TpcIO,TpcProcess] {

  import SpecHelper._

  def c(pid: Short) = new ProcessID(pid)

  Axiom("well-coordinated", P.exists( p => P.forall( q => p == q.coord)) )

  val spec = new Spec {
    val livenessPredicate = List[Formula](
      P.exists( p => P.forall( q => p == q.coord && p.HO.size == n && (q.HO contains p) ) )
    )
    val invariants = List[Formula](
      P.forall( p => p.decision == Some(true) ==> P.forall( q => q.vote )) && 
      P.forall( p => P.forall( q => (p.decision.isDefined && q.decision.isDefined) ==> (p.decision == q.decision) ))
    )
//  override val roundInvariants = List(
//    List(f(true)),
//    List(f(true))
//  )
    val properties = List[(String,Formula)](
      "Uniform Agreement" -> P.forall( p => P.forall( q => (p.decision.isDefined && q.decision.isDefined) ==> (p.decision == q.decision) )),
      //"Irrevocable" -> A site cannot reverse its decision after it has reached one. TODO need better handling of termination
      "Validity" -> P.forall( p => (p.decision == Some(true)) ==> P.forall( q => q.vote ) )
      //"Non-Triviality" -> If there are no fault and all sites voted Yes, then the decision will be to commit. TODO need to reason about HO in the whole run
      //"Termination" -> At any point in the execution of the protocol, if all existing failures are repaired and no new failures occur for sufficiently long, then all sites will eventually reach a decision. TODO we have a different fault model
    )
  }

  def process = new TpcProcess(timeout)

  def dummyIO = new TpcIO{
    val coord = new ProcessID(0)
    val canCommit = false
    def decide(value: Option[Boolean]) { }
  }
}


object TpcRunner extends RTOptions {
  

  var confFile = "src/test/resources/sample-conf.xml"
  
  val usage = "..."
  
  var rt: Runtime[TpcIO,TpcProcess] = null

  def defaultHandler(msg: Message) {
    msg.release
  }
  
  def main(args: Array[String]) {
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    val alg = new TwoPhaseCommit(timeout)
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
