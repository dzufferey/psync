package round.formula

sealed abstract class Type {
  def freeParameters: Set[TypeVariable]
  def alpha(subst: Map[TypeVariable, Type]): Type
  def arity = 0
  
  //syntactic sugar
  def ~>(that: Type): Function = this match {
    case Function(args, ret) => Function(args ::: List(ret), that)
    case other => Function(List(this), that)
  }
}

object Type {

  private val counter = new java.util.concurrent.atomic.AtomicInteger()

  def freshTypeVar = {
    val cnt = counter.incrementAndGet()
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
  def freeParameters = Set[TypeVariable]()
  def alpha(subst: Map[TypeVariable, Type]) = this 
}

case object Int extends Type {
  override def toString = "Int"
  def freeParameters = Set[TypeVariable]()
  def alpha(subst: Map[TypeVariable, Type]) = this 
}

case class FSet(arg: Type) extends Type {
  override def toString = "Set("+arg+")"
  def freeParameters = arg.freeParameters
  def alpha(subst: Map[TypeVariable, Type]) = FSet(arg.alpha(subst))
}

case class FOption(arg: Type) extends Type {
  override def toString = "Option("+arg+")"
  def freeParameters = arg.freeParameters
  def alpha(subst: Map[TypeVariable, Type]) = FOption(arg.alpha(subst))
}

case object Wildcard extends Type {
  override def toString = "_"
  def freeParameters = Set[TypeVariable]()
  def alpha(subst: Map[TypeVariable, Type]) = this 
}

case class Product(cmpts: List[Type]) extends Type {
  override def toString = cmpts.mkString("(","*",")")
  def freeParameters = (Set[TypeVariable]() /: cmpts)(_ ++ _.freeParameters)
  def alpha(subst: Map[TypeVariable, Type]) = Product(cmpts.map(_.alpha(subst))) 
}

case class Function(args: List[Type], returns: Type) extends Type {
  override def toString = args.mkString("(","->","->") + returns + ")"
  override def arity = args.length
  def freeParameters = (returns.freeParameters /: args)(_ ++ _.freeParameters)
  def alpha(subst: Map[TypeVariable, Type]) = Function(args.map(_.alpha(subst)), returns.alpha(subst)) 
}

case class UnInterpreted(id: String) extends Type {
  override def toString = id
  def freeParameters = Set[TypeVariable]()
  def alpha(subst: Map[TypeVariable, Type]) = this 
}

case class TypeVariable(name: String) extends Type {
  override def toString = "'"+name
  def freeParameters = Set[TypeVariable](this)
  def alpha(subst: Map[TypeVariable, Type]) = subst.getOrElse(this, this)
}

//TODO copier for Type

object UnitT {
  private val instance = Product(Nil)
  def apply() = instance
  def unapply(tpe: Type) = tpe match {
    case `instance` => true
    case _ => false
  }
}

