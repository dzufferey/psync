package psync.utils.isabelle

import edu.tum.cs.isabelle._
import edu.tum.cs.isabelle.api.Environment
import edu.tum.cs.isabelle.pure.{Expr => _, _}

object Operations {

  val startTheory = Operation.implicitly[(String,String), Unit]("start_theory")

  val prove = Operation.implicitly[(Term, Option[String]), Option[String]]("prove")

  val prettyPrint = Operation.implicitly[Term, String]("pretty_print")

}
