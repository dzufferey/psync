package round.macros

import round.formula._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait BoolExpr {
  self: Impl =>
  import c.universe._

  //TODO clean version using mirror ....
  def extractType(t: Type): round.formula.Type = {
    import definitions._
    if (t weak_<:< LongTpe) {
      Int
    } else if (t weak_<:< BooleanTpe) {
      Bool
    } else if (t == NoType) {
      Wildcard
    } else if (t.toString contains "Tuple"){
      println("TODO tuple: " + showRaw(t))
      Wildcard
    } else {
      //println("TODO type: Tuple, Set, ...")
      t match {
        case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "scala.Option" =>
          FOption(extractType(arg))
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
    }
  }

  //TODO
  // options

  def extractDomain(e: Tree): Option[Formula] = {
    if (e.tpe.typeConstructor.toString contains "Domain") { //TODO HACK!!
      None
    } else {
      Some(tree2Formula(e))
    }
  }


  def makeBinding(b: BindingType, domain: Tree, params: List[ValDef], body: Tree): Binding = {
    assert(params.length == 1)
    val x = params.head
    val t = extractType(x.tpe)
    val n = Variable(x.name.toString).setType(t)
    val f2 = tree2Formula(body)
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
      val rhs = tree2Formula(expr)
      val v = tname.toString
      val t = extractType(tpt.tpe)
      Eq(Variable(v).setType(t), rhs)
    case _ => sys.error("expected ValDef: " + showRaw(e))
  }

  def tree2Formula(e: Tree): Formula = {
    val formula: Formula = e match {
      // equality
      case q"$l == $r" => 
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Eq(l2,r2)
      case q"$l != $r" => 
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Neq(l2,r2)
      
      // inequality
      case q"$l <= $r" => 
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Leq(l2,r2)
      case q"$l >= $r" => 
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Geq(l2,r2)
      case q"$l < $r" => 
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Lt(l2,r2)
      case q"$l > $r" => 
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Gt(l2,r2)
     
      // arithmetic
      case q"$l + $r" => 
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Plus(l2,r2)
      case q"$l - $r" => 
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Minus(l2,r2)
      case q"$l * $r" => 
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Times(l2,r2)
      case q"$l / $r" => 
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Divides(l2,r2)
     
      // boolean expression
      case q"$l && $r" => 
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        And(l2,r2)
      case q"$l || $r" =>
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Or(l2,r2)
      case q"!$f" =>
        val f2 = tree2Formula(f)
        Not(f2)
      case q"$scope.SpecHelper.BoolOps($l).==>($r)" =>
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Implies(l2,r2)

      // set operation, comparison, cardinality
      case q"$s.size" =>
        val s2 = tree2Formula(s)
        Cardinality(s2)
      case q"$l union $r" =>
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Union(l2,r2)
      case q"$l intersect $r" =>
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Intersection(l2,r2)
      case q"$l subsetOf $r" =>
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        SubsetEq(l2,r2)
      case q"$l contains $r" =>
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        In(r2,l2)
     
      // quantifiers, comprehensions
      case q"$domain.forall( ..$xs => $f )" => makeBinding(ForAll, domain, xs, f)
      case q"$domain.exists( ..$xs => $f )" => makeBinding(Exists, domain, xs, f)
      case q"$domain.filter( ..$xs => $f )" => makeBinding(Comprehension, domain, xs, f)
     

      //uninterpreted fct
      case Apply(TypeApply(Select(Select(This(_), TermName("VarHelper")), TermName("getter")), List(TypeTree())), List(expr)) =>
        tree2Formula(expr)

      case q"$expr(..$args)" =>
        val fct = extractSymbol(expr)
        val args2 = args map tree2Formula
        Application(fct, args2)
      
      case q"$pkg.this.$expr" => //TODO does not seems right ...
        val n = expr.toString
        Variable(n)

      case q"$expr.$member" =>
        val fct: round.formula.Symbol = UnInterpretedFct(member.toString)
        val args = List(tree2Formula(expr))
        Application(fct, args)
     

      //literals and vars
      case Literal(Constant(v: Boolean)) => round.formula.Literal(v)
      case Literal(Constant(v: scala.Int)) => round.formula.Literal(v)
      case q"${ref: RefTree}" =>
        val n = ref.name.toString
        Variable(n)

      //defs
      case Block(defs, f) =>
        val f2 = tree2Formula(f)
        val d = defs map extractValDef
        d.foldLeft(f2)((x, y) => And(x, y))

      case other => sys.error("did not expect: " + showRaw(other))
    }

    val t = extractType(e.tpe)
    formula.setType(t)
  }

}
