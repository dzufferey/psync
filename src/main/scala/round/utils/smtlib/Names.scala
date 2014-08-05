package round.utils.smtlib

import round.formula._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object Names {
  
  def symbol(i: Symbol): String = i match {
    case Implies => "=>"
    case Or => "or"
    case And => "and"
    case Not => "not"
    case Eq => "="
    case Geq => ">="
    case Leq => "<="
    case Gt => ">"
    case Lt => "<"
    case Plus => "+"
    case Minus => "-"
    case Times => "*"
    case UnInterpretedFct(f, _, _) => f
    case Neq => Logger.logAndThrow("smtlib", Error, "≠ should be replaced by Not(Eq(...))")
    case i: InterpretedFct => i.symbol
  }

  def tpe(t: Type): String = t match {
    case Bool => "Bool"
    case Int => "Int"
    case Wildcard => "_"
    case FSet(elt) => sys.error("TODO FSet")
    case FOption(elt) => sys.error("TODO FOption")
    case Product(elts) => sys.error("TODO Product")
    case Function(args, returns) => args.map(tpe).mkString("(", ") (", ")") + " (" + tpe(returns) + ")"
    case UnInterpreted(id) => id
    case other => Logger.logAndThrow("smtlib", Error, "not supported: " + other)
  }
  
  def typeDecl(t: Type) = {
    val (args, ret) = t match {
      case Function(args, r) => (args, r)
      case other => (Nil, other)
    }
    val argsDecl = args.map(tpe).mkString("("," ",")")
    argsDecl + " " + tpe(ret)
  }
  
  val ite = {
    val fv = Type.freshTypeVar
    UnInterpretedFct("ite", Some(Bool ~> fv ~> fv ~> fv), List(fv))
  }

}
