package psync.macros

import psync.formula._
import psync.verification._

trait Lifting {
  self: Impl =>
  import c.universe._
  
  def _liftF(f: Formula): Tree = {
    val f1: Tree = f match {
      case psync.formula.Literal(x: Boolean) => q"psync.formula.Literal($x)"
      case psync.formula.Literal(x: Byte) => q"psync.formula.Literal($x)"
      case psync.formula.Literal(x: Int) => q"psync.formula.Literal($x)"
      case psync.formula.Literal(x: Long) => q"psync.formula.Literal($x)"
      case psync.formula.Literal(x: Short) => q"psync.formula.Literal($x)"
      case psync.formula.Literal(x: Char) => q"psync.formula.Literal($x)"
      case psync.formula.Literal(x) => sys.error("does not know how to lift: " + x)
      case Variable(n) => q"psync.formula.Variable($n)"
      case Application(sym, args) =>
        val sym2 = _liftS(sym)
        val args2 = args map _liftF
        q"psync.formula.Application($sym2, $args2)"
      case Binding(b, vs, f) =>
        val b2 = _liftBT(b)
        val vs2 = vs map _liftF
        val f2 = _liftF(f)
        q"psync.formula.Binding($b2, $vs2, $f2)"
    }
    if (f.tpe == Wildcard) {
      f1
    } else {
      val t = _liftT(f.tpe)
      q"$f1.setType($t)"
    }
  }

  def _liftS(s: psync.formula.Symbol): Tree = s match {
    case UnInterpretedFct(symbol, tpe, params) =>
      val params2 = params.map(_liftT)
      q"psync.formula.UnInterpretedFct($symbol, $tpe, $params2)"
    case Not => q"psync.formula.Not"
    case And => q"psync.formula.And"
    case Or => q"psync.formula.Or"
    case Implies => q"psync.formula.Implies"
    case Eq => q"psync.formula.Eq"
    case Neq => q"psync.formula.Neq"
    case Plus => q"psync.formula.Plus"
    case Minus => q"psync.formula.Minus"
    case Times => q"psync.formula.Times"
    case Divides => q"psync.formula.Divides"
    case Leq => q"psync.formula.Leq"
    case Geq => q"psync.formula.Geq"
    case Lt => q"psync.formula.Lt"
    case Gt => q"psync.formula.Gt"
    case Union => q"psync.formula.Union"
    case Intersection => q"psync.formula.Intersection"
    case SubsetEq => q"psync.formula.SubsetEq"
    case SupersetEq => q"psync.formula.SupersetEq"
    case In => q"psync.formula.In"
    case Contains => q"psync.formula.Contains"
    case Cardinality => q"psync.formula.Cardinality"
    case FSome => q"psync.formula.FSome"
    case FNone => q"psync.formula.FNone"
    case Get => q"psync.formula.Get"
    case IsDefined => q"psync.formula.IsDefined"
    case IsEmpty => q"psync.formula.IsEmpty"
    case Tuple => q"psync.formula.Tuple"
    case Fst => q"psync.formula.Fst"
    case Snd => q"psync.formula.Snd"
    case Trd => q"psync.formula.Trd"
    case KeySet => q"psync.formula.KeySet"
    case LookUp => q"psync.formula.LookUp"
    case IsDefinedAt => q"psync.formula.IsDefinedAt"
    case Size => q"psync.formula.Size"
  }

  def _liftBT(b: BindingType): Tree = b match {
    case ForAll => q"psync.formula.ForAll"
    case Exists => q"psync.formula.Exists"
    case Comprehension => q"psync.formula.Comprehension"
  }

  def _liftT(value: psync.formula.Type): Tree = value match {
    case Bool => q"psync.formula.Bool"
    case psync.formula.Int => q"psync.formula.Int"
    case Wildcard => q"psync.formula.Wildcard"
    case FSet(arg) =>
      val arg2 = _liftT(arg)
      q"psync.formula.FSet($arg2)"
    case FMap(k, v) =>
      val k2 = _liftT(k)
      val v2 = _liftT(v)
      q"psync.formula.FMap($k2, $v2)"
    case FOption(arg) =>
      val arg2 = _liftT(arg)
      q"psync.formula.FOption($arg2)"
    case Product(cmpts) =>
      val cmpts2 = cmpts map _liftT
      q"psync.formula.Product($cmpts2)"
    case psync.formula.Function(args, returns) =>
      val args2 = args map _liftT
      val returns2 = _liftT(returns)
      q"psync.formula.Function($args2,$returns2)"
    case UnInterpreted(id) => q"psync.formula.UnInterpreted($id)"
    case TypeVariable(name) => q"psync.formula.TypeVariable($name)"
  }

  def _liftTR(tr: TransitionRelation): Tree = {
    val tr2 = _liftF(tr.tr)
    val old = tr.old map _liftF
    val loc = tr.local map _liftF
    val primed = tr.primed.map(_liftF)
    q"new psync.verification.TransitionRelation($tr2, $old, $loc, $primed)"
  }
  
  def _liftRTR(tr: RoundTransitionRelation): Tree = {
    val send2 = _liftF(tr.send)
    val ms2 = _liftF(tr.mailboxSend)
    val updt2 = _liftF(tr.update)
    val mu2 = _liftF(tr.mailboxUpdt)
    val old = tr.old map _liftF
    val loc = tr.local map _liftF
    val primed = tr.primed.map(_liftF)
    q"new psync.verification.RoundTransitionRelation($send2, $ms2, $updt2, $mu2, $old, $loc, $primed)"
  }
  
  def _liftAX(aux: AuxiliaryMethod): Tree = {
    val name = aux.name
    val params = aux.params.map(_liftF)
    val tpe = _liftT(aux.tpe)
    val tParams = aux.tParams.map(_liftT)
    val pre = aux.pre
    val body = aux.body.map(_liftTR)
    val post = aux.post.map( p => (_liftF(p._1), _liftF(p._2)) )
    q"new psync.verification.AuxiliaryMethod($name, $params, $tpe, $tParams, $pre, $body, $post)"
  }

  implicit val liftF = new Liftable[Formula] {
    def apply(f: Formula) = _liftF(f)
  }
  
  implicit val liftS = new Liftable[psync.formula.Symbol] {
    def apply(s: psync.formula.Symbol) = _liftS(s)
  }

  implicit val liftBT = new Liftable[BindingType] {
    def apply(bt: BindingType) = _liftBT(bt)
  }
  
  implicit val liftT = new Liftable[psync.formula.Type] {
    def apply(t: psync.formula.Type) = _liftT(t)
  }
  
  implicit val liftTR = new Liftable[TransitionRelation] {
    def apply(tr: TransitionRelation) = _liftTR(tr)
  }

  implicit val liftRTR = new Liftable[RoundTransitionRelation] {
    def apply(tr: RoundTransitionRelation) = _liftRTR(tr)
  }

  implicit val liftAX = new Liftable[AuxiliaryMethod] {
    def apply(aux: AuxiliaryMethod) = _liftAX(aux)
  }
  
}
