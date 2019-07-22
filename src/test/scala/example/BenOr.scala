package example

import psync._
import psync.formula._
import psync.runtime._
import psync.macros.Macros._
import scala.language.existentials

//http://www.cs.utexas.edu/~lorenzo/corsi/cs380d/papers/p27-ben-or.pdf

class BenOrProcess(timeout: Long) extends Process[ConsensusIO[Boolean]] {
  
  var x = false
  var callback: ConsensusIO[Boolean] = null
  //to make the algorithm terminates as suggested in
  //http://www.cs.toronto.edu/~samvas/teaching/2221/handouts/benor-paper.pdf
  var canDecide = false
  var vote: Option[Boolean] = None
  
  var decision = false //TODO as ghost
  var decided =  false //TODO as ghost

  def init(io: ConsensusIO[Boolean]) = i{
    callback = io
    x = io.initialValue
    canDecide = false
    decided = false
  }
  
  val rounds = phase(
    new Round[(Boolean,Boolean)](timeout){
    
      def send: Map[ProcessID,(Boolean, Boolean)] = {
        broadcast( (x: Boolean) -> (canDecide: Boolean) )
      }

      def update(mailbox: Map[ProcessID,(Boolean, Boolean)]) {
        if (canDecide) {
          callback.decide(x)
          decided = true
          decision = x
          exitAtEndOfRound
        } else {
          if (mailbox.count(_._2._1) > n/2 || mailbox.exists(m => m._2._1 && m._2._2)) {
            vote = Some(true)
          } else if (mailbox.count(!_._2._1) > n/2 || mailbox.exists(m => !m._2._1 && m._2._2)) {
            vote = Some(false)
          } else {
            vote = None
          }
          canDecide = mailbox.exists(_._2._2)
        }
      }

    },
    
    new Round[Option[Boolean]](timeout){
    
      def send: Map[ProcessID,Option[Boolean]] = {
        broadcast( vote )
      }

      def update(mailbox: Map[ProcessID,Option[Boolean]]) {
        val t = mailbox.count{ case (p,v) => v == Some(true) }
        val f = mailbox.count{ case (p,v) => v == Some(false) }
        if (t > n/2) {
          x = true
          canDecide = true
        } else if (f > n/2) {
          x = false
          canDecide = true
        } else if (t > 1){
          x = true
        } else if (f > 1){
          x = false
        } else {
          x = util.Random.nextBoolean
        }
      }

    }
  )

}

class BenOr(rt: Runtime, timeout: Long) extends Algorithm[ConsensusIO[Boolean],BenOrProcess](rt) {

  import SpecHelper._

  val V = new Domain[Boolean]
  val spec = new Spec {
    override val safetyPredicate: Formula = P.forall( p => p.HO.size > n/2 ) //TODO might need something stronger like crash-fault
    val livenessPredicate = Nil
    val invariants = List[Formula](
        P.forall( i => !i.decided && !i.canDecide )
      ||
        V.exists( v => {
          val A = P.filter( i => i.x == v )
          A.size > n/2 &&
          P.forall( i => (i.decided ==> (i.decision == v) ) &&
                         (i.vote.isDefined ==> (i.vote == Some(v))) )
        })
    ) //TODO about canDecide ...
    override val roundInvariants = List(
      List[Formula](
        P.forall( p => p.vote.isDefined ==> P.filter( i => i.x == p.vote.get ).size > n/2 )
      )
    )
    val properties = List[(String,Formula)](
      ("Agreement",      P.forall( i => P.forall( j => (i.decided && j.decided) ==> (i.decision == j.decision) ))),
      ("Irrevocability", P.forall( i => old(i.decided) ==> (i.decided && old(i.decision) == i.decision) ))
      //TODO how to do non-triviality with random choice
      //no termination since we deal don't deal with probabilities
    )
  }

  def process = new BenOrProcess(timeout)

  def dummyIO = new ConsensusIO[Boolean]{
    val initialValue = false
    def decide(value: Boolean) { }
  }

}

object BenOrRunner extends Runner {
  
  def onStart {
    val alg = new BenOr(rt, timeout)
    import scala.util.Random
    val init = Random.nextBoolean
    val io = new ConsensusIO[Boolean] {
      val initialValue = init
      def decide(value: Boolean) {
        Console.println("replica " + id + " decided " + value)
      }
    }
    Thread.sleep(100)
    Console.println("replica " + id + " starting with " + init)
    alg.startInstance(0, io)
  }

}
