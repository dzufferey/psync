package round

import round.formula._

trait Specs[IO] {
  self: Algorithm[IO] =>

  abstract class Spec {
    val safetyPredicate: Formula
    val livnessPredicate: List[Formula]
    val invariants: List[Formula]
    val properties: List[(String, Formula)]
  }

  //TODO an axioms class for the verification ?

  object SpecHelper {
    implicit class BoolOps(lhs: Boolean) {
      def ==>(rhs: Boolean): Boolean =  !lhs || rhs
    }
  }

  var axiomList: List[Axiom] = Nil

  case class Axiom(name: String, formula: Formula) {
    axiomList = this :: axiomList
  }

}

