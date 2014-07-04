package round.formula

//TODO temporal aspect ??

sealed abstract class Formula {

  def toStringFull = "(" + toString + ": " + tpe + ")"

  var tpe: Type = Wildcard
  //TODO type checking

  def setType(t: Type): this.type = {
    tpe = t
    this
  }

  def alpha(map: Map[Variable, Variable]): Formula

  val freeVariables: Set[Variable]
  val boundVariables: Set[Variable]
}

case class Literal[T <: AnyVal](value: T) extends Formula {

  override def toString = value.toString

  def alpha(map: Map[Variable, Variable]) = this
  lazy val freeVariables = Set[Variable]()
  lazy val boundVariables = Set[Variable]()

}
object True {
  def unapply(f: Formula): Option[Unit] = f match {
    case Literal(true) => Some(())
    case _ => None
  }
  def apply(): Literal[Boolean] = Literal(true)
}
object False {
  def unapply(f: Formula): Option[Unit] = f match {
    case Literal(false) => Some(())
    case _ => None
  }
  def apply(): Literal[Boolean] = Literal(false)
}

case class Variable(name: String) extends Formula {

  override def toString = name

  def alpha(map: Map[Variable, Variable]) = map.getOrElse(this, this)
  lazy val freeVariables = Set[Variable](this)
  lazy val boundVariables = Set[Variable]()

}

case class Application(fct: Symbol, args: List[Formula]) extends Formula {

  override def toString = fct.toString + args.mkString("(",", ",")")

  def alpha(map: Map[Variable, Variable]) = Application(fct, args.map(_.alpha(map)))
  lazy val freeVariables = (Set[Variable]() /: args)(_ ++ _.freeVariables)
  lazy val boundVariables = (Set[Variable]() /: args)(_ ++ _.boundVariables)

}

object Fix extends Enumeration {
  type Fix = Value
  val Prefix, Infix, Suffix = Value
}

sealed abstract class Symbol {
  def tpe: Type
  val fix = Fix.Prefix
  val priority = 10
}

case class UnInterpretedFct(symbol: String,
                            _tpe: Option[Type] = None,
                            tParam: List[TypeVariable] = Nil) extends Symbol {
  override def toString = symbol

  def tpe = if (_tpe.isDefined) {
    val fvs = tParam.map( v => (v -> Type.freshTypeVar)).toMap
    _tpe.get alpha fvs
  } else {
    UnInterpreted(symbol)
  }
  override val priority = 20

}

sealed abstract class InterpretedFct(symbol: String, aliases: String*) extends Symbol {
  override def toString = symbol
  def apply(arg: Formula, args: Formula*): Formula = {
    val allArgs = (arg +: args).toList
    assert(allArgs.lengthCompare(tpe.arity) == 0)
    Application(this, allArgs)
    //TODO fill the type as much as possible
  }
  def unapply(f: Formula): Option[List[Formula]] = {
    val t = this
    f match {
      case Application(`t`, args) => Some(args)
      case _ => None
    }
  }
  override val fix = Fix.Infix
}

case object Not extends InterpretedFct("¬", "~", "!", "unary_!") {
  def tpe = Bool ~> Bool
  override val fix = Fix.Prefix
  override val priority = 8
}

case object And extends InterpretedFct("∧", "&&", "$amp$amp") {
  def tpe = Bool ~> Bool ~> Bool
  override val priority = 5
}
case object Or extends InterpretedFct("∨", "||") {
  def tpe = Bool ~> Bool ~> Bool
  override val priority = 4
}
case object Implies extends InterpretedFct("⇒", "==>", "$eq$eq$greater") {
  def tpe = Bool ~> Bool ~> Bool
  override val priority = 3
}

case object Eq extends InterpretedFct("=", "==", "⇔", "$eq$eq") {
  def tpe = {
    val fv = Type.freshTypeVar
    fv ~> fv ~> Bool
  }
  override val priority = 9
}
case object Neq extends InterpretedFct("≠", "!=", "~=") {
  def tpe = {
    val fv = Type.freshTypeVar
    fv ~> fv ~> Bool
  }
  override val priority = 9
}

case object Plus extends InterpretedFct("+") {
  def tpe = Int ~> Int ~> Int
  override val priority = 10
}
case object Minus extends InterpretedFct("-") {
  def tpe = Int ~> Int ~> Int
  override val priority = 12
}
case object Times extends InterpretedFct("∙", "*", "$times") {
  def tpe = Int ~> Int ~> Int
  override val priority = 15
}
case object Divides extends InterpretedFct("/", "$div") {
  def tpe = Int ~> Int ~> Int
  override val priority = 15
}

case object Leq extends InterpretedFct("≤", "<=") {
  def tpe = Int ~> Int ~> Bool
  override val priority = 9
}
case object Geq extends InterpretedFct("≥", ">=") {
  def tpe = Int ~> Int ~> Bool
  override val priority = 9
}
case object Lt extends InterpretedFct("<") {
  def tpe = Int ~> Int ~> Bool
  override val priority = 9
}
case object Gt extends InterpretedFct(">", "$greater") {
  def tpe = Int ~> Int ~> Bool
  override val priority = 9
}

case object Union extends InterpretedFct("∪", "|", "union") {
  def tpe = {
    val fv = Type.freshTypeVar
    FSet(fv) ~> FSet(fv) ~> FSet(fv)
  }
  override val priority = 10
}

case object Intersection extends InterpretedFct("∩", "intersect") {
  def tpe = {
    val fv = Type.freshTypeVar
    FSet(fv) ~> FSet(fv) ~> FSet(fv)
  }
  override val priority = 10
}

case object SubsetEq extends InterpretedFct("⊆", "subsetOf") {
  def tpe = {
    val fv = Type.freshTypeVar
    FSet(fv) ~> FSet(fv) ~> Bool
  }
  override val priority = 9
}

case object SupersetEq extends InterpretedFct("⊇") {
  def tpe = {
    val fv = Type.freshTypeVar
    FSet(fv) ~> FSet(fv) ~> Bool
  }
  override val priority = 9
}

case object In extends InterpretedFct("∈", "in") {
  def tpe = {
    val fv = Type.freshTypeVar
    fv ~> FSet(fv) ~> Bool
  }
  override val priority = 9
}

case object Contains extends InterpretedFct("∋", "contains") {
  def tpe = {
    val fv = Type.freshTypeVar
    FSet(fv) ~> fv ~> Bool
  }
  override val priority = 9
}

case object Cardinality extends InterpretedFct("card", "size") {
  def tpe = {
    val fv = Type.freshTypeVar
    FSet(fv) ~> Int
  }
  override val fix = Fix.Prefix
  override val priority = 20
}

case object FSome extends InterpretedFct("Some") {
  def tpe = {
    val fv = Type.freshTypeVar
    fv ~> FOption(fv)
  }
  override val fix = Fix.Prefix
  override val priority = 20
}

case object IsDefined extends InterpretedFct("isDefined") {
  def tpe = {
    val fv = Type.freshTypeVar
    FOption(fv) ~> Bool
  }
  override val fix = Fix.Prefix
  override val priority = 20
}

case object IsEmpty extends InterpretedFct("isEmpty") {
  def tpe = {
    val fv = Type.freshTypeVar
    FOption(fv) ~> Bool
  }
  override val fix = Fix.Prefix
  override val priority = 20
}

case object Get extends InterpretedFct("get") {
  def tpe = {
    val fv = Type.freshTypeVar
    FOption(fv) ~> fv
  }
  override val fix = Fix.Prefix
  override val priority = 20
}

case object Tuple extends InterpretedFct("") {
  def tpe = Wildcard ~> Wildcard
  override val fix = Fix.Prefix
  override val priority = 20
}
case object Fst extends InterpretedFct("_1") {
  def tpe = Wildcard ~> Wildcard
  override val fix = Fix.Suffix
  override val priority = 20
}
case object Snd extends InterpretedFct("_2") {
  def tpe = Wildcard ~> Wildcard
  override val fix = Fix.Suffix
  override val priority = 20
}
case object Trd extends InterpretedFct("_3") {
  def tpe = Wildcard ~> Wildcard
  override val fix = Fix.Suffix
  override val priority = 20
}


sealed abstract class BindingType

//TODO bound variables are in a set/domain:
//  as option since the type is the domain ...
//  or drop and put in the def
case class Binding(binding: BindingType, vs: List[Variable], f: Formula) extends Formula {

  override def toString = binding match {
    case Exists | ForAll => binding + " " + vs.mkString(""," ","") + ". " + f
    case Comprehension => "{ "+ vs.mkString(""," ","") + ". " + f + "}"
  }

  def alpha(map: Map[Variable, Variable]) = Binding(binding, vs, f.alpha(map -- vs))
  lazy val freeVariables = f.freeVariables -- vs
  lazy val boundVariables = f.boundVariables ++ vs

}

case object Comprehension extends BindingType {
  def unapply(f: Formula): Option[(List[Variable],Formula)] = f match {
    case Binding(Comprehension, v, f) => Some(v,f)
    case _ => None
  }
  def apply(vs:List[Variable], f: Formula) = {
    val fa = Binding(Comprehension, vs, f)
    val argsT = vs map (_.tpe)
    fa.tpe = FSet(argsT match {
      case List(t) => t
      case ts => Product(ts)
    })
    fa
  }
}

case object ForAll extends BindingType {
  def unapply(f: Formula): Option[(List[Variable],Formula)] = f match {
    case Binding(ForAll, v, f) => Some(v,f)
    case _ => None
  }
  def apply(vs:List[Variable], f: Formula) = {
    val fa = Binding(ForAll, vs, f)
    fa.tpe = Bool
    fa
  }
}
case object Exists extends BindingType {
  def unapply(f: Formula): Option[(List[Variable],Formula)] = f match {
    case Binding(Exists, v, f) => Some(v,f)
    case _ => None
  }
  def apply(vs:List[Variable], f: Formula) = {
    val ex = Binding(Exists, vs, f)
    ex.tpe = Bool
    ex
  }
}

//TODO extractor for SetEnum on top of comprehension
