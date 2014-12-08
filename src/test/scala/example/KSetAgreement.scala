package example

import round._
import round.macros.Macros._

class KSetAgreement(k: Int) extends Algorithm[ConsensusIO] {
  
  import VarHelper._
  import SpecHelper._

  
  val t = new LocalVariable[Map[ProcessID,Int]](Map.empty[ProcessID,Int])
  val decision = new LocalVariable[Int](-1) //TODO as ghost
  val decider = new LocalVariable[Boolean](false)
  //
  val callback = new LocalVariable[ConsensusIO](null)

  val spec = TrivialSpec
  //k-agreement: the set Y of decision values is such that Y ⊆ V₀ ∧ |Y| ≤ k
  //uncertainty: there exists a (k+1)-valent initial configuration

  //model, assumptions:
  // n > 2(k-1)
  // crash-fault, f < k
  // completely async (no termination requirement)

  def process = p(new Process[ConsensusIO]{

    def init(io: ConsensusIO) {
      callback <~ io
      decider <~ false
      //FIXME: crash in explicit outer in the scala compiler!!
      //t <~ Map(id -> io.initialValue)
    }

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
            callback.decide(pick(t))
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
