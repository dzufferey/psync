package round

import round.formula._

trait Specs[IO, P <: Process[IO]] {
  self: Algorithm[IO, P] =>

  abstract class Spec {
    val safetyPredicate: Formula = True()
    val livenessPredicate: List[Formula]
    /** phase invariant */
    val invariants: List[Formula]
    /** invariants for rounds */
    val roundInvariants: List[List[Formula]] = Nil
    val properties: List[(String, Formula)]
    /** Options for reducing Comprehension */
    val cl: logic.CL = new logic.CL( Some(2), None, Some(1))
  }

  object SpecHelper {
    implicit class BoolOps(lhs: Boolean) {
      def ==>(rhs: Boolean): Boolean =  !lhs || rhs
    }
    def init[T](v: T): T = sys.error("only for specification purpose, removed by macros")
    def old[T](v: T): T =  sys.error("only for specification purpose, removed by macros")
    def idToP(p: ProcessID): P =  sys.error("only for specification purpose, removed by macros")
  }

  var axiomList: List[Axiom] = Nil

  case class Axiom(name: String, formula: Formula) {
    axiomList = this :: axiomList
  }

  object TrivialSpec extends Spec {
    val livenessPredicate: List[Formula] = Nil
    val invariants: List[Formula] = Nil
    val properties: List[(String, Formula)] = Nil
  }

}

