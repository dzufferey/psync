package psync.logic

import psync.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object ReduceTime {

  val toInt   = UnInterpretedFct("toInt",   Some(Function(List(CL.timeType), Int)), Nil)
  val fromInt = UnInterpretedFct("newTime", Some(Function(List(Int), CL.timeType)), Nil)

  def change(t: Type): Type = t match {
    case t if t == CL.timeType => Int
    case Bool | Int | Wildcard | TypeVariable(_) | UnInterpreted(_) => t
    case FSet(t) => FSet(change(t))
    case FOption(t) => FOption(change(t))
    case FMap(k,v) => FMap(change(k),change(v))
    case Product(elts) => Product(elts.map(change))
    case Function(args, returns) => Function(args.map(change), change(returns))
  }

  //TODO should reuse terms if unchanged
  protected def copy(f: Formula): Formula = f match {
    case Application(t, List(time)) if t == toInt || t == fromInt => copy(time)
    case a @ Application(UnInterpretedFct(n, t, p), args) =>
      val t2 = t.map(change)
      val s = UnInterpretedFct(n, t2, p)
      val args2 = args.map(copy)
      s(args2:_*).setType(change(a.tpe))
    case a @ Application(s, args) =>
      val args2 = args.map(copy)
      s(args2:_*).setType(change(a.tpe))
    case v @ Variable(name) => Variable(name).setType(change(v.tpe))
    case l @ Literal(cnt) => Literal(cnt).setType(change(l.tpe))
    case b @ Binding(bs, vs, f) =>
      Binding(bs, vs.map(v => copy(v).asInstanceOf[Variable]), copy(f)).setType(change(b.tpe))
  }

  //TODO we must do a deep copy before changing the types!

  def apply(f: Formula): Formula = copy(f)
  
  def apply(fs: List[Formula]): List[Formula] = fs.map(apply)

}
