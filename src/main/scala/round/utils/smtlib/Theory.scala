package round.utils.smtlib

import round.formula._

sealed abstract class Theory
case object QF_UF extends Theory
case object UF extends Theory
case object QF_LIA extends Theory
case object LIA extends Theory
case object QF_UFLIA extends Theory
case object UFLIA extends Theory

object Theory {

  private def hasInt(t: Theory) = t match {
    case QF_LIA | LIA | QF_UFLIA | UFLIA => true
    case _ => false
  }

  def sort(t: Theory) = {
    if (hasInt(t)) {
      List(Bool, Int)
    } else {
      List(Bool)
    }
  }

  def fun(t: Theory) = {
    val base = List(And, Or, Not, Eq)
    val intF = List(Plus, Minus, Times, Leq, Lt, Geq, Gt)
    if (hasInt(t)) {
      base ::: intF
    } else {
      base
    }
  }

}
