package round.macros

import round.formula._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class Impl(val c: Context) {
  import c.universe._

  def _liftF(f: Formula): Tree = {
    val f1: Tree = f match {
      case round.formula.Literal(x: Boolean) => q"Literal($x)"
      case round.formula.Literal(x: Byte) => q"Literal($x)"
      case round.formula.Literal(x: Int) => q"Literal($x)"
      case round.formula.Literal(x: Long) => q"Literal($x)"
      case round.formula.Literal(x: Short) => q"Literal($x)"
      case round.formula.Literal(x: Char) => q"Literal($x)"
      case round.formula.Literal(x) => sys.error("does not know how to lift: " + x)
      case Variable(n) => q"Variable($n)"
      case Application(sym, args) =>
        val sym2 = _liftS(sym)
        val args2 = args map _liftF
        q"Application($sym2, $args2)"
      case Binding(b, vs, f) =>
        val b2 = _liftBT(b)
        val vs2 = vs map _liftF
        val f2 = _liftF(f)
        q"Binding($b2, $vs2, $f2)"
    }
    if (f.tpe == Wildcard) {
      f1
    } else {
      val t = _liftT(f.tpe)
      q"$f1.setType($t)"
    }
  }

  def _liftS(s: round.formula.Symbol): Tree = s match {
    case UnInterpretedFct(symbol) => q"UnInterpretedFct($symbol)"
    case Not => q"Not"
    case And => q"And"
    case Or => q"Or"
    case Implies => q"Implies"
    case Eq => q"Eq"
    case Neq => q"Neq"
    case Plus => q"Plus"
    case Minus => q"Minus"
    case Times => q"Times"
    case Divides => q"Divides"
    case Leq => q"Leq"
    case Geq => q"Geq"
    case Lt => q"Lt"
    case Gt => q"Gt"
    case Union => q"Union"
    case Intersection => q"Intersection"
    case SubsetEq => q"SubsetEq"
    case SupersetEq => q"SupersetEq"
    case In => q"In"
    case Contains => q"Contains"
    case Cardinality => q"Cardinality"
  }

  def _liftBT(b: BindingType): Tree = b match {
    case ForAll => q"ForAll"
    case Exists => q"Exists"
    case Comprehension => q"Comprehension"
  }

  def _liftT(value: round.formula.Type): Tree = value match {
    case Bool => q"Bool"
    case round.formula.Int => q"Int"
    case Wildcard => q"Wildcard"
    case FSet(arg) =>
      val arg2 = _liftT(arg)
      q"FSet($arg2)"
    case Product(cmpts) =>
      val cmpts2 = cmpts map _liftT
      q"Product($cmpts2)"
    case round.formula.Function(args, returns) =>
      val args2 = args map _liftT
      val returns2 = _liftT(returns)
      q"Function($args2,$returns2)"
    case UnInterpreted(id) => q"UnInterpreted($id)"
    case TypeVariable(name) => q"TypeVariable($name)"
    case FiniteValues(values) => sys.error("ToDo lifting FiniteValues")
  }

  
  implicit val liftF = new Liftable[Formula] {
    def apply(f: Formula) = _liftF(f)
  }
  
  implicit val liftS = new Liftable[round.formula.Symbol] {
    def apply(s: round.formula.Symbol) = _liftS(s)
  }

  implicit val liftBT = new Liftable[BindingType] {
    def apply(bt: BindingType) = _liftBT(bt)
  }
  
  implicit val liftT = new Liftable[round.formula.Type] {
    def apply(t: round.formula.Type) = _liftT(t)
  }
  
  def formula(e: c.Expr[Boolean]): c.Expr[Formula] = {

    //http://docs.scala-lang.org/overviews/quasiquotes/syntax-summary.html

    // recurse in function definition: 
    // q"(..$params) => $expr"
    //  Apply(Select(name,member), List(Function(List(args), body))
    //    Select(This(TypeName(...)), TermName(...))
    //    Select(Ident(TermName(...)), TermName(...))
    //user Transformer ?

  //def extractType(e: Tree): c.Expr[round.formula.Type] = {
  //  val tree = e match {
  //    case tq"scala.Int" => q"Int"
  //    case tq"scala.Boolean" => q"Bool"
  //    case tq"(..$tpts)" =>
  //      val args = tpts map extractType
  //      q"Product($args)"
  //    case tq"scala.collection.immutable.Set[..$tpts]" =>
  //      val st = extractType(tpts.head)
  //      q"Set($st)"
  //    case tq"(..$tpts) => scala.Boolean" if tpts.length == 1 =>
  //      val st = extractType(tpts.head)
  //      q"Set($st)"
  //   
  //    case tq"(..$tpts) => $tpt" =>
  //      val args = tpts map extractType
  //      val ret = extractType(tpt)
  //      q"round.formula.Function($args, $ret)" //do we want that ?
  //   
  //    case tq"${name: TypeName}" => sys.error("TODO, ident: " + e)
  //    case tq"$tpt#$tpname" => sys.error("TODO, projection: " + e)
  //    case tq"$ref.$tpname" => sys.error("TODO, selection: " + e)
  //    case tq"this.$tpname" => sys.error("TODO, this selection: " + e)
  //    case tq"$tpt[..$tpts]" => sys.error("TODO, applied type: " + e) //Domain ?
  //   
  //    case other => sys.error("do not support: " + other)
  //  }
  //  c.Expr[round.formula.Type](tree)
  //}

  //lazy val tuple2 = definitions.TupleClass(2).typeSignature
  //lazy val tuple3 = definitions.TupleClass(3).typeSignature
  //lazy val tuple4 = definitions.TupleClass(4).typeSignature
  lazy val option = definitions.OptionClass
  //lazy val set    = classOf[scala.collection.immutable.Set].typeSignature
    
    //TODO extract types from the scala tree
    //TODO Liftable[round.formula.Type]
    def extractType(t: Type): round.formula.Type = {
      import definitions._
      if (t weak_<:< LongTpe) {
        Int
      } else if (t weak_<:< BooleanTpe) {
        Bool
      } else if (t == NoType) {
        Wildcard
      } else if (t.toString contains "Option"){
        println("TODO Option: " + showRaw(t))
        Wildcard
      } else if (t.toString contains "Tuple"){
        println("TODO tuple: " + showRaw(t))
        Wildcard
      } else {
        //println("TODO type: Tuple, Set, ...")
        t match {
          case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "TypeName(\"Set\")" =>
            FSet(extractType(arg))
          case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "TypeName(\"LocalVariable\")" =>
            round.formula.Function(List(UnInterpreted("Process")), (extractType(arg)))
          case TypeRef(_, tRef, List()) if showRaw(tRef) == "TypeName(\"Process\")" =>
            UnInterpreted("Process")
          case MethodType(args, t2) => Wildcard
          case TypeRef(_, _, tUrl) => Wildcard
          case SingleType(NoPrefix, _) => Wildcard
          case SingleType(ThisType(_), _) => Wildcard
          case ThisType(_) => Wildcard
          case _ =>
            sys.error("not expected: " + showRaw(t))
        }
      //t match {
      //  case AppliedType(tpl, ts) if  tpl =:= tuple2 || tpl =:= tuple3 || tpl =:= tuple4 =>
      //    val args = ts map extractType
      //    q"Product($args)"
      //  case AppliedType(s, List(t)) if s =:= set =>
      //    val st = extractType(t)
      //    q"Set($st)"
      //  case AppliedType(`option`, List(t)) =>
      //    sys.error("TODO option")
      //  case _ => 
      //    sys.error("not expected: " + t)
      //}
      }
    }

    //TODO
    // options

    def extractDomain(e: Tree): Option[Formula] = {
      if (e.tpe.typeConstructor.toString contains "Domain") { //TODO HACK!!
        None
      } else {
        Some(traverse(e))
      }
    }


    def makeBinding(b: BindingType, domain: Tree, params: List[ValDef], body: Tree): Binding = {
      assert(params.length == 1)
      val x = params.head
      val t = extractType(x.tpe)
      val n = Variable(x.name.toString).setType(t)
      val f2 = traverse(body)
      val d = extractDomain(domain)
      b match {
        case Exists | Comprehension =>
          Binding(b, List(n), d.map( d => And(In(n,d),f2)).getOrElse(f2))
        case ForAll =>
          Binding(b, List(n), d.map( d => Implies(In(n,d),f2)).getOrElse(f2))
      }
    }

    def extractSymbol(e: Tree): round.formula.Symbol = e match {
      case Select(
                Apply(TypeApply(Select(Select(This(_), TermName("VarHelper")), TermName("init")), List(TypeTree())),
                List(Select(This(_), v))), TermName("apply")) =>
        UnInterpretedFct("__init__" + v.toString)
      case Select(
                Apply(TypeApply(Select(Select(This(_), TermName("VarHelper")), TermName("old")), List(TypeTree())),
                List(Select(This(_), v))), TermName("apply")) =>
        UnInterpretedFct("__old__" + v.toString)
      case q"${fct: RefTree}.apply" => UnInterpretedFct(fct.name.toString)
      case _ => sys.error("extractSymbol: " + showRaw(e))
    }

    def extractValDef(e: Tree): Formula = e match{
      case q"$mods val $tname: $tpt = $expr" =>
        val rhs = traverse(expr)
        val v = tname.toString
        val t = extractType(tpt.tpe)
        Eq(Variable(v).setType(t), rhs)
      case _ => sys.error("expected ValDef: " + showRaw(e))
    }

    //TODO typing
    def traverse(e: Tree): Formula = {
      val formula: Formula = e match {
        // equality
        case q"$l == $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          Eq(l2,r2)
        case q"$l != $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          Neq(l2,r2)
        
        // inequality
        case q"$l <= $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          Leq(l2,r2)
        case q"$l >= $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          Geq(l2,r2)
        case q"$l < $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          Lt(l2,r2)
        case q"$l > $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          Gt(l2,r2)
       
        // arithmetic
        case q"$l + $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          Plus(l2,r2)
        case q"$l - $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          Minus(l2,r2)
        case q"$l * $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          Times(l2,r2)
        case q"$l / $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          Divides(l2,r2)
       
        // boolean expression
        case q"$l && $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          And(l2,r2)
        case q"$l || $r" =>
          val l2 = traverse(l)
          val r2 = traverse(r)
          Or(l2,r2)
        case q"!$f" =>
          val f2 = traverse(f)
          Not(f2)
        case q"$scope.SpecHelper.BoolOps($l).==>($r)" =>
          val l2 = traverse(l)
          val r2 = traverse(r)
          Implies(l2,r2)

        // set operation, comparison, cardinality
        case q"$s.size" =>
          val s2 = traverse(s)
          Cardinality(s2)
        case q"$l union $r" =>
          val l2 = traverse(l)
          val r2 = traverse(r)
          Union(l2,r2)
        case q"$l intersect $r" =>
          val l2 = traverse(l)
          val r2 = traverse(r)
          Intersection(l2,r2)
        case q"$l subsetOf $r" =>
          val l2 = traverse(l)
          val r2 = traverse(r)
          SubsetEq(l2,r2)
        case q"$l contains $r" =>
          val l2 = traverse(l)
          val r2 = traverse(r)
          In(r2,l2)
       
        // quantifiers, comprehensions
        case q"$domain.forall( ..$xs => $f )" => makeBinding(ForAll, domain, xs, f)
        case q"$domain.exists( ..$xs => $f )" => makeBinding(Exists, domain, xs, f)
        case q"$domain.filter( ..$xs => $f )" => makeBinding(Comprehension, domain, xs, f)
       

        //uninterpreted fct
        case Apply(TypeApply(Select(Select(This(_), TermName("VarHelper")), TermName("getter")), List(TypeTree())), List(expr)) =>
          traverse(expr)

        case q"$expr(..$args)" =>
          val fct = extractSymbol(expr)
          val args2 = args map traverse
          Application(fct, args2)
        
        case q"$pkg.this.$expr" => //TODO does not seems right ...
          val n = expr.toString
          Variable(n)

        case q"$expr.$member" =>
          val fct: round.formula.Symbol = UnInterpretedFct(member.toString)
          val args = List(traverse(expr))
          Application(fct, args)
       

        //literals and vars
        case Literal(Constant(v: Boolean)) => round.formula.Literal(v)
        case Literal(Constant(v: scala.Int)) => round.formula.Literal(v)
        case q"${ref: RefTree}" =>
          val n = ref.name.toString
          Variable(n)

        //defs
        case Block(defs, f) =>
          val f2 = traverse(f)
          val d = defs map extractValDef
          d.foldLeft(f2)((x, y) => And(x, y))

        case other => sys.error("did not expect: " + showRaw(other))
      }

      val t = extractType(e.tpe)
      formula.setType(t)
    }

    val res = traverse(e.tree)
    val res2 =c.Expr[Formula](q"$res")
    //println(res2)
    res2
  }

}

object Macros {
  def f(e: Boolean): Formula = macro Impl.formula
}
