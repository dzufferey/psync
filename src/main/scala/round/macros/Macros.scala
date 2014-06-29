package round.macros

import round.formula._
import round._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
//import scala.reflect.macros.blackbox.Context

class Impl(val c: Context) extends Lifting
                           with BoolExpr
                           with ProcessRewrite 
                           with TrExtractor
{
  import c.universe._

  //http://docs.scala-lang.org/overviews/quasiquotes/syntax-summary.html

  def formula(e: c.Expr[Boolean]): c.Expr[Formula] = {
    val res = tree2Formula(e.tree)
    val res2 = c.Expr[Formula](q"$res")
    //println(res2)
    res2
  }

  //def process(e: c.Expr[Process]): c.Expr[Process] = {
  def process[T <: Process](e: c.Expr[T]): c.Expr[T] = {
    try {
      val res = processRewrite(e.tree)
      val res2 = c.Expr[T](q"$res")
      //println(res2)
      res2
    } catch {
      case e: Throwable =>
        e.printStackTrace
        c.abort(c.enclosingPosition, e.toString)
    }
  }
  
  def postprocessRound[T <: Round[_]](e: c.Expr[T]): c.Expr[T] = {
    try {
      val res = processRound(e.tree)
      val res2 = c.Expr[T](q"$res")
      //println(res2)
      res2
    } catch {
      case e: Throwable =>
        e.printStackTrace
        c.abort(c.enclosingPosition, e.toString)
    }
  }

}

object Macros {

  def f(e: Boolean): Formula = macro Impl.formula

  //def p(e: Process): Process = macro Impl.process
  def p[T <: Process](e: T): T = macro Impl.process[T]
  
  def rnd[T <: Round[_]](e: T): T = macro Impl.postprocessRound[T]

}
