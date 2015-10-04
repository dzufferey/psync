package round.verification

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox

object ReflectionUtils {

  import ru._

  def collectFields[T: TypeTag]: Iterable[String] = {
    typeOf[T].members.collect{
      //case m: TermSymbol if m.isSetter /*&& m.isPublic*/ =>
      case m: TermSymbol if m.isGetter /*&& m.isPublic*/ =>
        val sym = m.accessed.asInstanceOf[TermSymbol]
        sym.toString
    }
  }

}
