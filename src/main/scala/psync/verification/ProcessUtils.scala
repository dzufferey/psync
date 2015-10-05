package psync.verification

import Utils._

import psync._
import psync.formula._

import scala.reflect.runtime.universe.TypeTag

object ProcessUtils {

  def globalVariables = Set(
    Variable("r").setType(timeType),
    Variable("n").setType(Int)
  )

  def localVariables[T: TypeTag] = {
    val syms = ReflectionUtils.collectFields[T]
    val vars = syms.map(Variable(_)).toSet -- processVariableToIgnore
    vars ++ frameWorkVariables
  }
  
  /** variable defined in Process/RtProcess which are not directly accessible */
  protected val processVariableToIgnore = Set(
    Variable("_id").setType(procType),
    Variable("rr").setType(timeType),
    Variable("_r").setType(Int),
    Variable("_n").setType(Int),
    Variable("HO").setType(FSet(procType))
  )

  protected val frameWorkVariables = Set(
    Variable("id").setType(procType),
    Variable("HO").setType(FSet(procType))
  )

}
