package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer

//the definition of a comprehension
//TODO trim the scope when the body is defined
case class SetDef(scope: Set[Variable], id: Formula, body: Option[Binding]) {
    
  Logger.assert(id.tpe match { case FSet(_) => true; case _ => false }, "SetDef", "SetDef had not FSet type: " + id.tpe + "\n" + id + " = " + body)

  def tpe = id.tpe

  def contentTpe = tpe match {
    case FSet(t) => t
    case other => Logger.logAndThrow("SetDef", Error, "SetDef had not FSet type: " + other + "\n" + id + " = " + body)
  }

  def fresh: SetDef = {
    val newScope = scope.map(v => v -> Variable(Namer(v.name)).setType(v.tpe)).toMap
    val scope1 = newScope.values.toSet
    val id1 = FormulaUtils.alpha(newScope, id)
    val body1 = body.map(FormulaUtils.alpha(newScope, _).asInstanceOf[Binding])
    SetDef(scope1, id1, body1)
  }

  //TODO also take the scope in the normalization
  def normalize: SetDef = {
    val newBody = body.map( Simplify.deBruijnIndex(_).asInstanceOf[Binding] )
    val n = SetDef(scope, id, newBody)
    //Logger("SetDef", Debug, "before: " + this)
    //Logger("SetDef", Debug, "after:  " + n)
    n
  }

  //assume normalized
  def similar(sd: SetDef) = {
    scope == sd.scope && body == sd.body
  }

}

