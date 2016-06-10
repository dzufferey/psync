package psync.utils.isabelle

import edu.tum.cs.isabelle._
import edu.tum.cs.isabelle.api.Environment
import edu.tum.cs.isabelle.pure.{Expr => _, _}

object Operations {

  val prove = Operation.implicitly[(Term, Option[String]), Option[String]]("prove")

}
