package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer

//the definition of a comprehension
//TODO trim the scope when the body is defined
case class SetDef(scope: Set[Variable], id: Formula, body: Option[Binding]) {

  def tpe = id.tpe

  def contentTpe = tpe match {
    case FSet(t) => t
    case other => Logger.logAndThrow("SetDef", Error, "SetDef had not FSet type: " + other + "\n" + id + " = " + body)
  }

  def fresh: SetDef = {
    val newScope = scope.map(v => v -> Variable(Namer(v.name)).setType(v.tpe)).toMap
    SetDef(newScope.values.toSet, id.alpha(newScope), body.map(_.alpha(newScope)))
  }

  def normalize: SetDef = {
    val newBody = body.map( Simplify.deBruijnIndex(_).asInstanceOf[Binding] )
    val n = SetDef(scope, id, newBody)
    //Logger("SetDef", Debug, "before: " + this)
    //Logger("SetDef", Debug, "after:  " + n)
    n
  }

}

