package example

import round._
import round.macros.Macros._

class KSetAgreement(k: Int) extends Algorithm[ConsensusIO] {
  
  import VarHelper._
  import SpecHelper._

  
  val t = new LocalVariable[Map[ProcessID,Int]](Map.empty[ProcessID,Int])
  val decision = new LocalVariable[Int](-1) //TODO as ghost
  val decider = new LocalVariable[Boolean](false)

  val spec = TrivialSpec
  //k-agreement: the set Y of decision values is such that Y ⊆ V₀ ∧ |Y| ≤ k
  //uncertainty: there exists a (k+1)-valent initial configuration

  //model, assumptions:
  // n > 2(k-1)
  // crash-fault, f < k
  // completely async (no termination requirement)

  def process(_id: ProcessID, io: ConsensusIO) = p(new Process(_id) {

    decider <~ false
    t <~ Map(_id -> io.initialValue) //TODO id makes the compiler crash! (value <none> not element of example.KSetAgreement)

    val rounds = Array[Round](
      rnd(new Round{
      
        type A = (Boolean, Map[ProcessID,Int])

        def merge(a: Map[ProcessID,Int], b: Map[ProcessID,Int]) = {
          a ++ b
        }

        def pick(a: Map[ProcessID,Int]) = a.values.min

        def send: Set[((Boolean, Map[ProcessID,Int]),ProcessID)] = {
          broadcast( (decider: Boolean) -> (t: Map[ProcessID,Int]) )
        }

        def update(mailbox: Set[((Boolean, Map[ProcessID,Int]), ProcessID)]) {
          val content = mailbox.map(_._1)
          if (decider) {
            io.decide(pick(t))
            terminate()
          } else if (content.exists(_._1)) {
            decider <~ true
            t <~ content.find(_._1).get._2
          } else {
            val same = mailbox.filter(_._1._2 == (t: Map[ProcessID, Int]))
            if (same.size > n - k) {
              decider <~ true
            } else {
              for (((_,v),_) <- mailbox)
                t <~ merge(t, v)
            }
          }
        }

      })
    )
  })
}
