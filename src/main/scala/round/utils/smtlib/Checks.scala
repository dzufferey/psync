package round.utils.smtlib

import round.formula._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object Checks {

  def apply(f: Formula) {
    Logger("smtlib", Debug, "sanity checks for " + f)
    val traverser = new Traverser {
      override def traverse(f: Formula) = {
        super.traverse(f)
        f match {
          case c @ Comprehension(_, _) =>
            Logger.logAndThrow("smtlib.Checks", Error, "Comprehension should be reduces (see CL): " + c)
          case _ => ()
        }
        f.tpe match {
          case v @ TypeVariable(_) =>
            Logger.logAndThrow("smtlib.Checks", Error, "type variables: " + v + " type of " + f)
          case Wildcard =>
            Logger.logAndThrow("smtlib.Checks", Error, "wildcard type: " + f)
          case _ => ()
        }
      }
    }
    traverser.traverse(f)
  }

}
