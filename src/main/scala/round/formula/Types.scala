package round.formula

import scala.collection.immutable.{Set => SSet}

sealed abstract class Type {
  def freeParameters: SSet[TypeVariable]
  def alpha(subst: Map[TypeVariable, Type]): Type
  
  //syntactic sugar
  def ~>(that: Type): Function = this match {
    case Function(args, ret) => Function(args ::: List(ret), that)
    case other => Function(List(this), that)
  }
}

object Type {

  var cnt = 0

  //TODO synchronise
  def freshTypeVar = {
    cnt += 1
    TypeVariable("_" + cnt)
  }

  def freshParams(tpe: Type): (Map[TypeVariable,TypeVariable], Type) = {
    var oldParams = tpe.freeParameters
    var subst: Map[TypeVariable,TypeVariable] = (for (t <- oldParams.toSeq) yield (t, freshTypeVar)).toMap
    (subst, tpe alpha subst)
  }

}

case object Bool extends Type {
  override def toString = "Bool"
  def freeParameters = SSet[TypeVariable]()
  def alpha(subst: Map[TypeVariable, Type]) = this 
}

case object Int extends Type {
  override def toString = "Int"
  def freeParameters = SSet[TypeVariable]()
  def alpha(subst: Map[TypeVariable, Type]) = this 
}

case class Set(arg: Type) extends Type {
  override def toString = "Set("+arg+")"
  def freeParameters = arg.freeParameters
  def alpha(subst: Map[TypeVariable, Type]) = Set(arg.alpha(subst))
}

case object Wildcard extends Type {
  override def toString = "_"
  def freeParameters = SSet[TypeVariable]()
  def alpha(subst: Map[TypeVariable, Type]) = this 
}

case class Product(cmpts: List[Type]) extends Type {
  override def toString = cmpts.mkString("","*","")
  def freeParameters = (SSet[TypeVariable]() /: cmpts)(_ ++ _.freeParameters)
  def alpha(subst: Map[TypeVariable, Type]) = Product(cmpts.map(_.alpha(subst))) 
}

case class Function(args: List[Type], returns: Type) extends Type {
  override def toString = args.mkString("(","->","->") + returns + ")"
  def freeParameters = (returns.freeParameters /: args)(_ ++ _.freeParameters)
  def alpha(subst: Map[TypeVariable, Type]) = Function(args.map(_.alpha(subst)), returns.alpha(subst)) 
}

case class FiniteValues[T](values: List[T]) extends Type {
  override def toString = values.mkString("{",",","}")
  def freeParameters = SSet[TypeVariable]()
  def alpha(subst: Map[TypeVariable, Type]) = this 
}

case class UnInterpreted(id: String) extends Type {
  override def toString = id
  def freeParameters = SSet[TypeVariable]()
  def alpha(subst: Map[TypeVariable, Type]) = this 
}

case class TypeVariable(name: String) extends Type {
  override def toString = "'"+name
  def freeParameters = SSet[TypeVariable](this)
  def alpha(subst: Map[TypeVariable, Type]) = subst.getOrElse(this, this)
}

//TODO copier for Type

//TODO accessor for tuples

//TODO Nothing types ?

object UnitT {
  private val instance = FiniteValues(List( () ))
  def apply(): FiniteValues[Unit] = instance
  def unapply(tpe: FiniteValues[Unit]) = tpe match {
    case FiniteValues(List( () )) => true
    case _ => false
  }
}

