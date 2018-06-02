package psync.macros

import psync.formula._
import psync._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
//import scala.reflect.macros.blackbox.Context

class Impl(val c: Context) extends Lifting
                           with TypeExtractor
                           with FormulaExtractor
                           with SSA
                           with TrExtractor
                           with ProcessRewrite 
                           with RoundRewrite
{
  import c.universe._

  //http://docs.scala-lang.org/overviews/quasiquotes/syntax-summary.html

  def formula(e: c.Expr[Boolean]): c.Expr[Formula] = {
    val res = getConstraints(e.tree)
    val res2 = c.Expr[Formula](q"$res")
    //println(res2)
    res2
  }
  
  def any2Formula[T](any: c.Expr[T]): c.Expr[Formula] = {
    val res = getConstraints(any.tree)
    val res2 = c.Expr[Formula](q"$res")
    //println(res2)
    res2
  }

  def init/*[IO](io: c.Expr[IO])*/(e: c.Expr[Unit]): c.Expr[Unit] = {
    try {
      val res = extractInit(/*io.tree,*/ e.tree)
      val res2 = c.Expr[Unit](q"$res")
      //println(res2)
      res2
    } catch {
      case t: Throwable =>
        t.printStackTrace
        c.warning(c.enclosingPosition, "when extracting initial state formula: " + t.toString)
        e
    }
  }

  def postprocessRound[A](e: c.Expr[Round[A]]): c.Expr[RtRound] = {
    try {
      val res = processRound(e.tree)
      val res2 = c.Expr[RtRound](q"$res")
      //println(res2)
      res2
    } catch {
      case e: Throwable =>
        e.printStackTrace
        c.abort(c.enclosingPosition, e.toString)
    }
  }

  def mkPhase(e: c.Expr[Round[_]]*): c.Expr[Array[RtRound]] = {
    try {
      val rounds = e.map( expr => processRound(expr.tree) )
      val array = q"Array[RtRound](..$rounds)"
      val res = c.Expr[Array[RtRound]](array)
      //println(res)
      res
    } catch {
      case e: Throwable =>
        e.printStackTrace
        c.abort(c.enclosingPosition, e.toString)
    }
  }

}

object Macros {

  def f(e: Boolean): Formula = macro Impl.formula
  implicit def booleanToFormula(e: Boolean): Formula = macro Impl.formula

  //def p[T <: Process[_]](e: T): T = macro Impl.process[T]
  
  def i/*[IO](io: IO)*/(e: Unit): Unit = macro Impl.init//[IO]
  
  //def rnd[T <: Round](e: T): T = macro Impl.postprocessRound[T]
  def rnd[A](e: Round[A]): RtRound = macro Impl.postprocessRound[A]

  def phase(e: Round[_]*): Array[RtRound] = macro Impl.mkPhase

  def asFormula[T](any: Any): Formula = macro Impl.any2Formula[T]

}
