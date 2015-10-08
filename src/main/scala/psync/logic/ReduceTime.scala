package psync.logic

import psync.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object ReduceTime {

  val toInt   = UnInterpretedFct("toInt",   Some(Function(List(CL.timeType), Int)), Nil)
  val fromInt = UnInterpretedFct("newTime", Some(Function(List(Int), CL.timeType)), Nil)

  def change(t: Type): Type = t match {
    case Bool | Int | Wildcard | TypeVariable(_) | UnInterpreted(_) => t
    case FSet(t) => FSet(change(t))
    case FOption(t) => FOption(change(t))
    case FMap(k,v) => FMap(change(k),change(v))
    case Product(elts) => Product(elts.map(change))
    case Function(args, returns) => Function(args.map(change), change(returns))
  }

  //TODO we must do a deep copy before changing the types!

  def apply(f: Formula): Formula = {
    // remove the conversions
    val fixSymbol = FormulaUtils.map({
      case Application(t, List(time)) if t == toInt || t == fromInt => time
      case Application(UnInterpretedFct(n, t, p), args)  =>
        val t2 = t.map(change)
        val s = UnInterpretedFct(n, t2, p)
        s(args:_*)
      case other => other
    }, f)
    // replace timeType by Int
    FormulaUtils.traverse( x => {
      if (x.tpe == CL.timeType) x.setType(Int) }, fixSymbol )
    //
    fixSymbol
  }
  
  def apply(fs: List[Formula]): List[Formula] = fs.map(apply)

}
