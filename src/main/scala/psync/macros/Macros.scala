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

  def postprocessRound(e: c.Expr[RtRound]): c.Expr[(RtRound,RoundSpec)] = {
    try {
      val rnd, spec = processRound(e.tree)
      val res2 = c.Expr[(RtRound,RoundSpec)](q"($rnd, $spec)")
      //println(res2)
      res2
    } catch {
      case e: Throwable =>
        e.printStackTrace
        c.abort(c.enclosingPosition, e.toString)
    }
  }

  def mkPhase(e: c.Expr[RtRound]*): c.Expr[Array[(RtRound,RoundSpec)]] = {
    try {
      val rounds = e.map( expr => processRound(expr.tree) )
      val array = q"Array[(RtRound,RoundSpec)](..$rounds)"
      val res = c.Expr[Array[(RtRound,RoundSpec)]](array)
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
  
  def rnd(e: RtRound): (RtRound,RoundSpec) = macro Impl.postprocessRound

  def phase(e: RtRound*): Array[(RtRound,RoundSpec)] = macro Impl.mkPhase

  def asFormula[T](any: Any): Formula = macro Impl.any2Formula[T]

}
