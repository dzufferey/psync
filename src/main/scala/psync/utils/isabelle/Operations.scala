package psync.utils.isabelle

import info.hupel.isabelle._
import info.hupel.isabelle.api.Environment
import info.hupel.isabelle.pure.{Expr => _, _}

object Operations {

  val startTheory = Operation.implicitly[(String,String), Unit]("start_theory")

  val prove = Operation.implicitly[(Term, Option[String]), Option[String]]("prove")

  val prettyPrint = Operation.implicitly[Term, String]("pretty_print")

}
