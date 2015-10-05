package psync.utils.smtlib

import psync.formula._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object Checks {

  def apply(f: Formula) {
    Logger("smtlib", Debug, "sanity checks for " + f)
    val traverser = new Traverser {
      override def traverse(f: Formula) = {
        super.traverse(f)
        f match {
          case v @ Variable(_) =>
            Logger("smtlib", Debug, v + ": " + v.tpe)
          case a @ Application(fct, args) =>
            Logger("smtlib", Debug, fct + "(" + args.map(_.tpe).mkString(",") + "): " + a.tpe +
                                           " with " + fct.typeWithParams + ", " + fct.typeParams)
          case c @ Comprehension(_, _) =>
            Logger.logAndThrow("smtlib", Error, "Checks, Comprehension should have been reduced (see CL): " + c)
          case _ => ()
        }
        f.tpe match {
          case v @ TypeVariable(_) =>
            Logger.logAndThrow("smtlib", Error, "Checks, type variables: " + v + " type of " + f)
          case Wildcard =>
            Logger.logAndThrow("smtlib", Error, "Checks, wildcard type: " + f)
          case _ => ()
        }
      }
    }
    traverser.traverse(f)
  }

}
