package round.utils.smtlib

import round.formula._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object Checks {

  def apply(f: Formula) {
    Logger("smtlib.checks", Debug, "sanity checks for " + f)
    val traverser = new Traverser {
      override def traverse(f: Formula) = {
        super.traverse(f)
        f match {
          case v @ Variable(_) =>
            Logger("smtlib.checks", Debug, v + ": " + v.tpe)
          case a @ Application(fct, args) =>
            Logger("smtlib.checks", Debug, fct + "(" + args.map(_.tpe).mkString(",") + "): " + a.tpe +
                                           " with " + fct.typeWithParams + ", " + fct.typeParams)
          case c @ Comprehension(_, _) =>
            Logger.logAndThrow("smtlib.checks", Error, "Comprehension should be reduces (see CL): " + c)
          case _ => ()
        }
        f.tpe match {
          case v @ TypeVariable(_) =>
            Logger.logAndThrow("smtlib.checks", Error, "type variables: " + v + " type of " + f)
          case Wildcard =>
            Logger.logAndThrow("smtlib.checks", Error, "wildcard type: " + f)
          case _ => ()
        }
      }
    }
    traverser.traverse(f)
  }

}
