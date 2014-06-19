package round.macros

import round.formula._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait Lifting {
  self: Impl =>
  import c.universe._
  
  def _liftF(f: Formula): Tree = {
    val f1: Tree = f match {
      case round.formula.Literal(x: Boolean) => q"Literal($x)"
      case round.formula.Literal(x: Byte) => q"Literal($x)"
      case round.formula.Literal(x: Int) => q"Literal($x)"
      case round.formula.Literal(x: Long) => q"Literal($x)"
      case round.formula.Literal(x: Short) => q"Literal($x)"
      case round.formula.Literal(x: Char) => q"Literal($x)"
      case round.formula.Literal(x) => sys.error("does not know how to lift: " + x)
      case Variable(n) => q"Variable($n)"
      case Application(sym, args) =>
        val sym2 = _liftS(sym)
        val args2 = args map _liftF
        q"Application($sym2, $args2)"
      case Binding(b, vs, f) =>
        val b2 = _liftBT(b)
        val vs2 = vs map _liftF
        val f2 = _liftF(f)
        q"Binding($b2, $vs2, $f2)"
    }
    if (f.tpe == Wildcard) {
      f1
    } else {
      val t = _liftT(f.tpe)
      q"$f1.setType($t)"
    }
  }

  def _liftS(s: round.formula.Symbol): Tree = s match {
    case UnInterpretedFct(symbol) => q"UnInterpretedFct($symbol)"
    case Not => q"Not"
    case And => q"And"
    case Or => q"Or"
    case Implies => q"Implies"
    case Eq => q"Eq"
    case Neq => q"Neq"
    case Plus => q"Plus"
    case Minus => q"Minus"
    case Times => q"Times"
    case Divides => q"Divides"
    case Leq => q"Leq"
    case Geq => q"Geq"
    case Lt => q"Lt"
    case Gt => q"Gt"
    case Union => q"Union"
    case Intersection => q"Intersection"
    case SubsetEq => q"SubsetEq"
    case SupersetEq => q"SupersetEq"
    case In => q"In"
    case Contains => q"Contains"
    case Cardinality => q"Cardinality"
  }

  def _liftBT(b: BindingType): Tree = b match {
    case ForAll => q"ForAll"
    case Exists => q"Exists"
    case Comprehension => q"Comprehension"
  }

  def _liftT(value: round.formula.Type): Tree = value match {
    case Bool => q"Bool"
    case round.formula.Int => q"Int"
    case Wildcard => q"Wildcard"
    case FSet(arg) =>
      val arg2 = _liftT(arg)
      q"FSet($arg2)"
    case FOption(arg) =>
      val arg2 = _liftT(arg)
      q"FOption($arg2)"
    case Product(cmpts) =>
      val cmpts2 = cmpts map _liftT
      q"Product($cmpts2)"
    case round.formula.Function(args, returns) =>
      val args2 = args map _liftT
      val returns2 = _liftT(returns)
      q"Function($args2,$returns2)"
    case UnInterpreted(id) => q"UnInterpreted($id)"
    case TypeVariable(name) => q"TypeVariable($name)"
    case FiniteValues(values) => sys.error("ToDo lifting FiniteValues")
  }

  
  implicit val liftF = new Liftable[Formula] {
    def apply(f: Formula) = _liftF(f)
  }
  
  implicit val liftS = new Liftable[round.formula.Symbol] {
    def apply(s: round.formula.Symbol) = _liftS(s)
  }

  implicit val liftBT = new Liftable[BindingType] {
    def apply(bt: BindingType) = _liftBT(bt)
  }
  
  implicit val liftT = new Liftable[round.formula.Type] {
    def apply(t: round.formula.Type) = _liftT(t)
  }
  
}
