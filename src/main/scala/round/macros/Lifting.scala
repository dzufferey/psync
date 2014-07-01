package round.macros

import round.formula._
import round.verification._

trait Lifting {
  self: Impl =>
  import c.universe._
  
  def _liftF(f: Formula): Tree = {
    val f1: Tree = f match {
      case round.formula.Literal(x: Boolean) => q"round.formula.Literal($x)"
      case round.formula.Literal(x: Byte) => q"round.formula.Literal($x)"
      case round.formula.Literal(x: Int) => q"round.formula.Literal($x)"
      case round.formula.Literal(x: Long) => q"round.formula.Literal($x)"
      case round.formula.Literal(x: Short) => q"round.formula.Literal($x)"
      case round.formula.Literal(x: Char) => q"round.formula.Literal($x)"
      case round.formula.Literal(x) => sys.error("does not know how to lift: " + x)
      case Variable(n) => q"round.formula.Variable($n)"
      case Application(sym, args) =>
        val sym2 = _liftS(sym)
        val args2 = args map _liftF
        q"round.formula.Application($sym2, $args2)"
      case Binding(b, vs, f) =>
        val b2 = _liftBT(b)
        val vs2 = vs map _liftF
        val f2 = _liftF(f)
        q"round.formula.Binding($b2, $vs2, $f2)"
    }
    if (f.tpe == Wildcard) {
      f1
    } else {
      val t = _liftT(f.tpe)
      q"$f1.setType($t)"
    }
  }

  def _liftS(s: round.formula.Symbol): Tree = s match {
    case UnInterpretedFct(symbol, tpe, params) =>
      val params2 = params.map(_liftT)
      q"round.formula.UnInterpretedFct($symbol, $tpe, $params2)"
    case Not => q"round.formula.Not"
    case And => q"round.formula.And"
    case Or => q"round.formula.Or"
    case Implies => q"round.formula.Implies"
    case Eq => q"round.formula.Eq"
    case Neq => q"round.formula.Neq"
    case Plus => q"round.formula.Plus"
    case Minus => q"round.formula.Minus"
    case Times => q"round.formula.Times"
    case Divides => q"round.formula.Divides"
    case Leq => q"round.formula.Leq"
    case Geq => q"round.formula.Geq"
    case Lt => q"round.formula.Lt"
    case Gt => q"round.formula.Gt"
    case Union => q"round.formula.Union"
    case Intersection => q"round.formula.Intersection"
    case SubsetEq => q"round.formula.SubsetEq"
    case SupersetEq => q"round.formula.SupersetEq"
    case In => q"round.formula.In"
    case Contains => q"round.formula.Contains"
    case Cardinality => q"round.formula.Cardinality"
    case FSome => q"round.formula.FSome"
    case Get => q"round.formula.Get"
    case IsDefined => q"round.formula.IsDefined"
    case IsEmpty => q"round.formula.IsEmpty"
  }

  def _liftBT(b: BindingType): Tree = b match {
    case ForAll => q"round.formula.ForAll"
    case Exists => q"round.formula.Exists"
    case Comprehension => q"round.formula.Comprehension"
  }

  def _liftT(value: round.formula.Type): Tree = value match {
    case Bool => q"round.formula.Bool"
    case round.formula.Int => q"round.formula.Int"
    case Wildcard => q"round.formula.Wildcard"
    case FSet(arg) =>
      val arg2 = _liftT(arg)
      q"round.formula.FSet($arg2)"
    case FOption(arg) =>
      val arg2 = _liftT(arg)
      q"round.formula.FOption($arg2)"
    case Product(cmpts) =>
      val cmpts2 = cmpts map _liftT
      q"round.formula.Product($cmpts2)"
    case round.formula.Function(args, returns) =>
      val args2 = args map _liftT
      val returns2 = _liftT(returns)
      q"round.formula.Function($args2,$returns2)"
    case UnInterpreted(id) => q"round.formula.UnInterpreted($id)"
    case TypeVariable(name) => q"round.formula.TypeVariable($name)"
    case FiniteValues(values) => sys.error("ToDo lifting FiniteValues")
  }

  def _liftTR(tr: TransitionRelation): Tree = {
    val tr2 = _liftF(tr.tr)
    val old = tr.old map _liftF
    val primed = tr.primed.map(_liftF)
    q"new round.verification.TransitionRelation($tr2, $old, $primed)"
  }
  
  def _liftAX(aux: AuxiliaryMethod): Tree = {
    val name = aux.name
    val params = aux.params.map(_liftF)
    val tpe = _liftT(aux.tpe)
    val tParams = aux.tParams.map(_liftT)
    val pre = aux.pre
    val body = aux.body.map(_liftTR)
    val post = (_liftF(aux.post._1), _liftF(aux.post._2))
    q"new round.verification.AuxiliaryMethod($name, $params, $tpe, $tParams, $pre, $body, $post)"
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
  
  implicit val liftTR = new Liftable[TransitionRelation] {
    def apply(tr: TransitionRelation) = _liftTR(tr)
  }

  implicit val liftAX = new Liftable[AuxiliaryMethod] {
    def apply(aux: AuxiliaryMethod) = _liftAX(aux)
  }
  
}
