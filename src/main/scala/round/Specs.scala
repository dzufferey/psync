package round

import round.formula._

trait Specs[IO] {
  self: Algorithm[IO] =>

  abstract class Spec {
    val safetyPredicate: Formula = True()
    val livenessPredicate: List[Formula]
    /** phase invariant */
    val invariants: List[Formula]
    /** invariants for rounds */
    val roundInvariants: List[List[Formula]] = Nil
    val properties: List[(String, Formula)]
  }

  object SpecHelper {
    implicit class BoolOps(lhs: Boolean) {
      def ==>(rhs: Boolean): Boolean =  !lhs || rhs
    }
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

