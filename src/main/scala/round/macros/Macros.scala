package round.macros

import round.formula._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class Impl(val c: Context) {
  import c.universe._
  
  implicit val liftS = Liftable[round.formula.Symbol] {
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

  implicit val liftBT = Liftable[BindingType] {
    case ForAll => q"ForAll"
    case Exists => q"Exists"
    case Comprehension => q"Comprehension"
  }
  
  implicit val liftT = new Liftable[round.formula.Type] {
    def apply(value: round.formula.Type): Tree = value match {
      case Bool => q"Bool"
      case round.formula.Int => q"Int"
      case Wildcard => q"Wildcard"
      case FSet(arg) =>
        val arg2 = apply(arg)
        q"FSet($arg2)"
      case Product(cmpts) =>
        val cmpts2 = cmpts map apply
        q"Product($cmpts2)"
      case round.formula.Function(args, returns) =>
        val args2 = args map apply
        val returns2 = apply(returns)
        q"Function($args2,$returns2)"
      case UnInterpreted(id) => q"UnInterpreted($id)"
      case TypeVariable(name) => q"TypeVariable($name)"
      case FiniteValues(values) => sys.error("ToDo lifting FiniteValues")
    }
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
  //lazy val option = definitions.OptionClass.typeSignature
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
      } else {
        t match {
          case MethodType(args, t2) => Wildcard
          case TypeRef(_, _, tUrl) => Wildcard
          case SingleType(NoPrefix, _) => Wildcard
          case ThisType(_) => Wildcard
          case _ => sys.error("not expected: " + showRaw(t))
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
    // valdef
    // lift Formula
    // ..

    def makeBinding(b: BindingType, domain: Tree, params: List[ValDef], body: Tree): Tree = {
      assert(params.length == 1)
      val x = params.head
      val n = x.name.toString
      val t = extractType(x.tpe)
      val f2 = traverse(body)
      //TODO domain
      q"Binding($b, List(Variable($n).setType($t)), $f2)"
    }

    def extractSymbol(e: Tree): round.formula.Symbol = e match {
      case q"${fct: RefTree}.apply" => UnInterpretedFct(fct.name.toString)
      case _ => sys.error("extractSymbol: " + showRaw(e))
    }

    //TODO typing
    def traverse(e: Tree): c.Expr[Formula] = {
      val formula = e match {
        // equality
        case q"$l == $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"Eq($l2,$r2)"
        case q"$l != $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"Neq($l2,$r2)"
        
        // inequality
        case q"$l <= $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"Leq($l2,$r2)"
        case q"$l >= $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"Geq($l2,$r2)"
        case q"$l < $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"Lt($l2,$r2)"
        case q"$l > $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"Gt($l2,$r2)"
       
        // arithmetic
        case q"$l + $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"Plus($l2,$r2)"
        case q"$l - $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"Minus($l2,$r2)"
        case q"$l * $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"Times($l2,$r2)"
        case q"$l / $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"Divides($l2,$r2)"
       
        // boolean expression
        case q"$l && $r" => 
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"And($l2,$r2)"
        case q"$l || $r" =>
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"Or($l2,$r2)"
        case q"!$f" =>
          val f2 = traverse(f)
          q"Not($f2)"
        case q"this.SpecHelper.BoolOps($l).==>($r)" =>
          val l2 = traverse(l)
          val r2 = traverse(r)
          q"Implies($l2,$r2)"

        // set operation, comparison, cardinality
       
        // quantifiers, comprehensions
        case q"$domain.forall( ..$xs => $f )" => makeBinding(ForAll, domain, xs, f)
        case q"$domain.exists( ..$xs => $f )" => makeBinding(Exists, domain, xs, f)
        case q"$domain.filter( ..$xs => $f )" => makeBinding(Comprehension, domain, xs, f)
       

        //uninterpreted fct
        case Apply(TypeApply(Select(Select(This(_), TermName("VarHelper")), TermName("getter")), List(TypeTree())), List(expr)) =>
          //println("getter 1: " + e)
          //println("getter 2: " + expr)
          traverse(expr).tree

        case q"$expr(..$args)" =>
          //println("apply 1: " + expr)
          //println("apply 2: " + args)
          val fct = extractSymbol(expr)
          val args2 = args map traverse
          q"Application($fct, $args2)"
        
        case q"$pkg.this.$expr" => //TODO does not seems right ...
          //println("this: " + expr)
          val n = expr.toString
          q"Variable($n)"

        case q"$expr.$member" =>
          val fct: round.formula.Symbol = UnInterpretedFct(member.toString)
          val args = List(traverse(expr))
          q"Application($fct, $args)"
       

        //literals and vars
        case Literal(Constant(v: Boolean)) => q"Literal($v)"
        case Literal(Constant(v: scala.Int)) => q"Literal($v)"
        case q"${ref: RefTree}" =>
          //println("ref: " + ref)
          val n = ref.name.toString
          q"Variable($n)"

        //defs
        case Block(defs, f) =>
          sys.error("TODO")

        case other => sys.error("did not expect: " + showRaw(other))
      }

      val t = extractType(e.tpe)
      c.Expr[Formula](q"$formula.setType($t)")
    }

    val res = traverse(e.tree)
    //println(res)
    res
  }

}

object Macros {
  def f(e: Boolean): Formula = macro Impl.formula
}
