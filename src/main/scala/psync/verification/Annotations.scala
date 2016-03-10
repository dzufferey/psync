package psync.verification

import psync.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import scala.annotation.StaticAnnotation
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox


final case class requires(f: Formula) extends StaticAnnotation
final case class ensures(result: String, f: Formula) extends StaticAnnotation

object Annotations {

  private val reqTpe = ru.typeOf[requires]
  private val ensTpe = ru.typeOf[ensures]

  private val mirror = ru.runtimeMirror(getClass.getClassLoader)
  private val tb = mirror.mkToolBox()


  def eval(a: ru.Annotation) = {
    assert(a.tree.tpe == reqTpe || a.tree.tpe == ensTpe, "wrong type: " + a.tree.tpe)
    val str = a.toString
    Logger("Annotations", Info, "recontructing: " + str)
    //TODO this a big hack!!
    val expr = tb.eval(tb.parse(str)) // linter:ignore UndesirableTypeInference
    Logger("Annotations", Info, "result: " + expr)
    expr
  }

}
