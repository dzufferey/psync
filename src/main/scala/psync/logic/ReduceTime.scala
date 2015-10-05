package psync.logic

import psync.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object ReduceTime {

  val toInt   = UnInterpretedFct("toInt",   Some(Function(List(CL.timeType), Int)), Nil)
  val fromInt = UnInterpretedFct("newTime", Some(Function(List(Int), CL.timeType)), Nil)

  def apply(f: Formula): Formula = {
    // remove the conversions
    val noConversion = FormulaUtils.map({
      case Application(t, List(time)) if t == toInt || t == fromInt => time
      case other => other
    }, f)
    // replace timeType by Int
    FormulaUtils.traverse( x => {
      if (x.tpe == CL.timeType) x.setType(Int) }, noConversion )
    //
    noConversion
  }
  
  def apply(fs: List[Formula]): List[Formula] = fs.map(apply)

}
