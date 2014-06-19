package round.macros

import round.formula._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class Impl(val c: Context) extends Lifting
                           with BoolExpr
{
  import c.universe._

  //http://docs.scala-lang.org/overviews/quasiquotes/syntax-summary.html

  def formula(e: c.Expr[Boolean]): c.Expr[Formula] = {
    val res = tree2Formula(e.tree)
    val res2 = c.Expr[Formula](q"$res")
    //println(res2)
    res2
  }

}

object Macros {

  def f(e: Boolean): Formula = macro Impl.formula

  //TODO what about the code inside the Round
  //-need to locate the send and update part
  //-make it SSA
  //-turn it into a formula

  //TODO the user defined method inside the Process
  //if they have some pre/post condition, we need to extract and verify them
}
