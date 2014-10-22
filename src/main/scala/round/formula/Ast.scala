package round.formula

//TODO temporal aspect ??

sealed abstract class Formula {

  def toStringFull = "(" + toString + ": " + tpe + ")"

  var tpe: Type = Wildcard

  def setType(t: Type): this.type = {
    tpe = t
    this
  }

  def alpha(map: Map[Variable, Variable]): Formula

  val freeVariables: Set[Variable]
  val boundVariables: Set[Variable]
}

case class Literal[T <: AnyVal](value: T) extends Formula {

  value match {
    case _: Boolean => tpe = Bool
    case _: scala.Int => tpe = Int
    case _: scala.Long => tpe = Int
    case _: scala.Short => tpe = Int
    case _: scala.Byte => tpe = Int
    case _ => ()
  }

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

  fct.tpe match {
    case Function(_, ret) => tpe = ret
    case _ => ()
  }

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
  val typeParams: List[TypeVariable] = Nil
  val typeWithParams: Type

  def instanciateType(ts: List[Type]) = {
    val subst = typeParams.zip(ts).toMap
    typeWithParams alpha subst
  }

  def tpe: Type = instanciateType(typeParams.map( t => Type.freshTypeVar))

  val fix = Fix.Prefix
  val priority = 10
}

case class UnInterpretedFct(symbol: String,
                            _tpe: Option[Type] = None,
                            tParam: List[TypeVariable] = Nil) extends Symbol {
  override def toString = symbol
  
  def raw = symbol + tParam.mkString("[",",","]") + ": " + _tpe.map(_.toString).getOrElse("--")

  def stripType = UnInterpretedFct(symbol)

  override val typeParams = tParam
  val typeWithParams = _tpe match { 
    case Some(t) => t
    case None => Wildcard
  }

  override val priority = 20

}

sealed abstract class InterpretedFct(val symbol: String, aliases: String*) extends Symbol {
  override def toString = symbol

  def allSymbols = symbol +: aliases

  def arity = tpe.arity

  def apply(arg: Formula, args: Formula*): Formula = {
    val allArgs = (arg +: args).toList
    assert(allArgs.lengthCompare(arity) == 0)
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

case object Not extends InterpretedFct("¬", "~", "!", "unary_!", "unary_$bang") {
  val typeWithParams = Bool ~> Bool
  override val fix = Fix.Prefix
  override val priority = 8
}

case object And extends InterpretedFct("∧", "&&", "$amp$amp", "and") {
  val typeWithParams = Bool ~> Bool ~> Bool
  override val priority = 5
}
case object Or extends InterpretedFct("∨", "||", "$bar$bar", "or") {
  val typeWithParams = Bool ~> Bool ~> Bool
  override val priority = 4
}
case object Implies extends InterpretedFct("⇒", "==>", "$eq$eq$greater", "=>") {
  val typeWithParams = Bool ~> Bool ~> Bool
  override val priority = 3
}

case object Eq extends InterpretedFct("=", "==", "⇔", "$eq$eq") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = fv ~> fv ~> Bool
  override val priority = 9
}
case object Neq extends InterpretedFct("≠", "!=", "~=", "$bang$eq") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = fv ~> fv ~> Bool
  override val priority = 9
}

case object Plus extends InterpretedFct("+", "$plus") {
  val typeWithParams = Int ~> Int ~> Int
  override val priority = 10
}
case object Minus extends InterpretedFct("-", "$minus") {
  val typeWithParams = Int ~> Int ~> Int
  override val priority = 12
}
case object Times extends InterpretedFct("∙", "*", "$times") {
  val typeWithParams = Int ~> Int ~> Int
  override val priority = 15
}
case object Divides extends InterpretedFct("/", "$div") {
  val typeWithParams = Int ~> Int ~> Int
  override val priority = 15
}

case object Leq extends InterpretedFct("≤", "<=", "$less$eq") {
  val typeWithParams = Int ~> Int ~> Bool
  override val priority = 9
}
case object Geq extends InterpretedFct("≥", ">=", "$greater$eq") {
  val typeWithParams = Int ~> Int ~> Bool
  override val priority = 9
}
case object Lt extends InterpretedFct("<", "$less") {
  val typeWithParams = Int ~> Int ~> Bool
  override val priority = 9
}
case object Gt extends InterpretedFct(">", "$greater") {
  val typeWithParams = Int ~> Int ~> Bool
  override val priority = 9
}

case object Union extends InterpretedFct("∪", "|", "union") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = FSet(fv) ~> FSet(fv) ~> FSet(fv)
  override val priority = 10
}

case object Intersection extends InterpretedFct("∩", "intersect") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = FSet(fv) ~> FSet(fv) ~> FSet(fv)
  override val priority = 10
}

case object SubsetEq extends InterpretedFct("⊆", "subsetOf") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = FSet(fv) ~> FSet(fv) ~> Bool
  override val priority = 9
}

case object SupersetEq extends InterpretedFct("⊇") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = FSet(fv) ~> FSet(fv) ~> Bool
  override val priority = 9
}

case object In extends InterpretedFct("∈", "in") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = fv ~> FSet(fv) ~> Bool
  override val priority = 9
}

case object Contains extends InterpretedFct("∋", "contains") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = FSet(fv) ~> fv ~> Bool
  override val priority = 9
}

case object Cardinality extends InterpretedFct("card", "size") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = FSet(fv) ~> Int
  override val fix = Fix.Prefix
  override val priority = 20
}

case object FSome extends InterpretedFct("Some") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = fv ~> FOption(fv)
  override val fix = Fix.Prefix
  override val priority = 20
}
case object FNone extends InterpretedFct("None") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = FOption(fv)
  override val fix = Fix.Prefix
  override val priority = 20
}
case object IsDefined extends InterpretedFct("isDefined") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = FOption(fv) ~> Bool
  override val fix = Fix.Prefix
  override val priority = 20
}
case object IsEmpty extends InterpretedFct("isEmpty") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = FOption(fv) ~> Bool
  override val fix = Fix.Prefix
  override val priority = 20
}
case object Get extends InterpretedFct("get") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = FOption(fv) ~> fv
  override val fix = Fix.Prefix
  override val priority = 20
}

case object Tuple extends InterpretedFct("") {
  val typeWithParams = Wildcard ~> Wildcard
  override val fix = Fix.Prefix
  override val priority = 20
}
case object Fst extends InterpretedFct("_1") {
  val typeWithParams = Wildcard ~> Wildcard
  override val fix = Fix.Suffix
  override val priority = 20
}
case object Snd extends InterpretedFct("_2") {
  val typeWithParams = Wildcard ~> Wildcard
  override val fix = Fix.Suffix
  override val priority = 20
}
case object Trd extends InterpretedFct("_3") {
  val typeWithParams = Wildcard ~> Wildcard
  override val fix = Fix.Suffix
  override val priority = 20
}

object InterpretedFct {
  private var symbols: List[InterpretedFct] = Nil
  private var map: Map[String,InterpretedFct] = Map.empty
  def add(s: InterpretedFct) {
    symbols = s :: symbols
    map = s.allSymbols.foldLeft(map)( (m, sym) => {
      assert(!(map contains sym), "symbol redefinition: " + sym)
      m + (sym -> s)
    })
    assert(s.allSymbols.forall(map contains _))
  }
  def apply(s: String): Option[InterpretedFct] = {
    map.get(s)
  }
  def knows(s: String) = {
    val res = map contains s
    //println(s + " -> " + res)
    res
  }

  //need to be added manually since object are initialized lazily
  add( Not )
  add( And )
  add( Or )
  add( Implies )
  add( Eq )
  add( Neq )
  add( Plus )
  add( Minus )
  add( Times )
  add( Divides )
  add( Leq )
  add( Geq )
  add( Lt )
  add( Gt )
  add( Union )
  add( Intersection )
  add( SubsetEq )
  add( SupersetEq )
  add( In )
  add( Contains )
  add( Cardinality )
  add( FSome )
  add( FNone )
  add( IsDefined )
  add( IsEmpty )
  add( Get )
  add( Tuple )
  add( Fst )
  add( Snd )
  add( Trd )
}


sealed abstract class BindingType

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
  def apply(vs:List[Variable], f: Formula) = f match {
    case ForAll(vs2, f2) => Binding(ForAll, vs ::: vs2, f2)
    case _ => if (vs.isEmpty) f else Binding(ForAll, vs, f).setType(Bool)
  }
}
case object Exists extends BindingType {
  def unapply(f: Formula): Option[(List[Variable],Formula)] = f match {
    case Binding(Exists, v, f) => Some(v,f)
    case _ => None
  }
  def apply(vs:List[Variable], f: Formula) = f match {
    case Exists(vs2, f2) => Binding(Exists, vs ::: vs2, f2)
    case _ => if (vs.isEmpty) f else Binding(Exists, vs, f).setType(Bool)
  }
}

//TODO extractor for SetEnum on top of comprehension
