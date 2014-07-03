package round.macros

import round.formula._
import round.utils.Namer

trait BoolExpr {
  self: Impl =>
  import c.universe._

  def isTuple(t: Symbol) = {
    showRaw(t) startsWith "scala.Tuple" //TODO
  }

  //TODO clean version using mirror ....
  def extractType(t: Type): round.formula.Type = {
    import definitions._
    if (t == null) {
      Wildcard
    } else if (t weak_<:< LongTpe) {
      Int
    } else if (t weak_<:< BooleanTpe) {
      Bool
    } else if (t == NoType) {
      Wildcard
    } else {
      t match {
        case TypeRef(_, tRef, args) if isTuple(tRef) =>
          Product(args map extractType)
        case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "scala.Option" =>
          FOption(extractType(arg))
        case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "TypeName(\"Set\")" =>
          FSet(extractType(arg))
        case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "TypeName(\"LocalVariable\")" =>
          round.formula.Function(List(round.verification.Utils.procType), (extractType(arg)))
        case TypeRef(_, TypeName("ProcessID"), List()) =>
          round.verification.Utils.procType
        case other =>
          //TODO
          //println("extractType:\n  " + other + "\n  " + showRaw(other))
          Wildcard
      }
    }
  }
  
  // what about type alias ?
  
  //TODO
  def extractType(t: Tree): round.formula.Type = t match {
    case TypeTree() => extractType(t.tpe)
    case Ident(TypeName("Int"))
       | Ident(TypeName("Long"))
       | Ident(TypeName("Short")) => Int
    case Ident(TypeName("Boolean")) => Bool
    case AppliedTypeTree(Ident(TypeName("Option")), List(tpe)) => FOption(extractType(tpe))
    case AppliedTypeTree(Ident(TypeName("Set")), List(tpe)) => FSet(extractType(tpe))
    case Ident(TypeName("ProcessID")) => round.verification.Utils.procType
    case Select(Ident(pkg), TypeName(tn)) => UnInterpreted(pkg.toString + "." + tn)
    case _ => sys.error("TODO extractType from Tree: " + showRaw(t))
  }
  
  //TODO
  def extractTypeVar(t: Tree): round.formula.TypeVariable = {
    sys.error("TODO extractTypeFromTypeTree: " + showRaw(t))
  }

  def extractDomain(e: Tree): Option[Formula] = {
    if (e.tpe.typeConstructor.toString contains "Domain") { //TODO
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
    case TypeApply(Select(Select(Ident(scala), TermName("Some")), TermName("apply")), List(tpt)) =>
      FSome
    //TODO tuple
    case q"${fct: RefTree}.apply" => UnInterpretedFct(fct.name.toString)
    case q"${fct: RefTree}.$fct2" => UnInterpretedFct(fct.name.toString + "_" + fct2.toString)
    case q"$pkg.this.$fct" => UnInterpretedFct(/*pkg.name.toString + "_" +*/ fct.toString)
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
  
  def extractVarFromValDef(e: Tree): Variable = e match{
    case q"$mods val $tname: $tpt = $expr" =>
      val v = tname.toString
      val t = extractType(tpt.tpe)
      Variable(v).setType(t)
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
      case q"$domain.map[$tpt1,$tpt2]( $x => $f )(immutable.this.Set.canBuildFrom[$tpt3])" =>
        // { y | x ∈ domain ∧ y = f(x) }
        val y = Ident(TermName(c.freshName("y")))
        val yF = Variable(y.toString).setType(extractType(tpt1))
        val fCstr = makeConstraints(f, Some(y), Some(y))
        val dCstr = In(extractVarFromValDef(x), tree2Formula(domain))
        Comprehension(List(yF), And(dCstr, fCstr))

      case Apply(TypeApply(Select(Select(This(_), TermName("VarHelper")), TermName("getter")), List(TypeTree())), List(expr)) =>
        tree2Formula(expr)

      //provided by the framework
      //TODO make sure it is not used somewhere else
      case q"$pkg.this.broadcast($expr)" =>
        val payload = tree2Formula(expr)
        val msg = Variable(Namer("__msg")).setType(Product(List(payload.tpe, round.verification.Utils.procType)))
        val fst = Application(UnInterpretedFct("_1"), List(msg)) //TODO tuple projection ...
        Comprehension(List(msg), Eq(fst, payload))

      //(un)interpreted fct
      case q"$expr(..$args)" =>
        val fct = extractSymbol(expr)
        val args2 = args map tree2Formula
        Application(fct, args2)
      
      case fld @ q"$pkg.this.$expr" => //TODO does not seems right ...
        val n = expr.toString
        Variable(n).setType(extractType(fld.tpe))

      case q"$expr.$member" =>
        val fct: round.formula.Symbol = UnInterpretedFct(member.toString)
        val args = List(tree2Formula(expr))
        Application(fct, args)
     
      case Typed(e, _) => //TODO good enough ?
        tree2Formula(e)

      //literals and vars
      case Literal(Constant(v: Boolean)) => round.formula.Literal(v)
      case Literal(Constant(v: scala.Int)) => round.formula.Literal(v)
      case q"${ref: RefTree}" =>
        val n = ref.name.toString
        Variable(n).setType(extractType(ref.tpe))

      //defs
      case Block(defs, f) =>
        val f2 = tree2Formula(f)
        val d = defs map extractValDef
        d.foldLeft(f2)((x, y) => And(x, y))

      case Literal(Constant(())) => True() //TODO should be a decent subtitute

      case EmptyTree => True() //TODO should be a decent subtitute

      case other => sys.error("did not expect:\n" + other + "\n" + showRaw(other))
    }

    val t = extractType(e.tpe)
    formula.setType(t)
  }

  /* constraints for a loop-free block in SSA. */
  def makeConstraints(
      body: Tree,
      currRet: Option[Tree] = None,
      globalRet: Option[Tree] = None
    ): Formula =
  {
    body match {
     
      case If(cond, thenp, elsep) =>
        val condCstr = makeConstraints(cond, None, None)
        val thenCstr = makeConstraints(thenp, currRet, globalRet)
        val elseCstr = makeConstraints(elsep, currRet, globalRet)
        Or(And(condCstr, thenCstr), And(Not(condCstr), elseCstr))
     
      case Block(stats, expr) =>
        val statsCstr = stats.map(makeConstraints(_, None, globalRet))
        val retCstr = makeConstraints(expr, currRet, globalRet)
        statsCstr.foldRight(retCstr)(And(_,_))
     
      case Return(expr) =>
        globalRet.map( ret => makeConstraints(Assign(ret, expr))).getOrElse(True())
      
      case Typed(e, _) =>
        makeConstraints(e, currRet, globalRet)
      
      case Apply(Select(lhs, TermName("$less$tilde")), List(rhs)) =>
        Eq(tree2Formula(lhs), tree2Formula(rhs))
      case Assign(lhs, rhs) =>
        Eq(tree2Formula(lhs), tree2Formula(rhs))
     
      case ValDef(mods, name, tpe, rhs) =>
        makeConstraints(rhs, Some(Ident(name)), globalRet)
     
      case term: TermTree => 
        makeConstraints(Assign(currRet.getOrElse(Ident(TermName(Namer("dummy")))), term))
     
      case term: RefTree =>
        makeConstraints(Assign(currRet.getOrElse(Ident(TermName(Namer("dummy")))), term))
      
      case Match(selector, cases) =>
        c.abort(c.enclosingPosition, "pattern matching an algebraic datatypes not yet supported")
      //for loops
      case LabelDef(name, params, rhs) =>
        c.abort(c.enclosingPosition, "while loop not yet supported")
      //case Apply(id @ Ident(_), paramss) if id.symbol.isLabel =>
      //  c.abort(c.enclosingPosition, "loop not yet supported")
      case Try(block, catches, finalizer) =>
        c.abort(c.enclosingPosition, "try/catch yet supported")
      case Throw(expr) =>
        c.abort(c.enclosingPosition, "throwing exception not yet supported")
     
      case other =>
        c.abort(c.enclosingPosition, "makeConstraints, did not expect: " + other)
    }
  }

}
