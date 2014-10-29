package round.macros

import round.formula._
import round.verification._
import dzufferey.utils.Namer

trait FormulaExtractor {
  self: Impl =>
  import c.universe._

  
  object IsTuple {
    def unapply(t: Type): Option[List[Type]] = t match {
      case TypeRef(_, tRef, args) if showRaw(tRef) startsWith "scala.Tuple" => Some(args)
      case _ => None
    }
  }

  object IsSet {
    def unapply(t: Type): Option[Type] = t match {
      case TypeRef(_, tRef, List(arg)) =>
        val sr = showRaw(tRef)
        if (sr.startsWith("scala.collection.immutable.Set") || 
            sr == "TypeName(\"Set\")") {
          Some(arg)
        } else {
          None
        }
      case _ => None
    }
  }

  object IsOption {
    def unapply(t: Type): Option[Type] = t match {
      case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "scala.Option" => Some(arg)
      case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "scala.Some" => Some(arg)
      case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "scala.None" => Some(arg)
      case _ => None
    }
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
        case IsTuple(args) => Product(args map extractType)
        case IsSet(arg) => FSet(extractType(arg))
        case IsOption(arg) =>  FOption(extractType(arg))
        case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "TypeName(\"LocalVariable\")" =>
          round.formula.Function(List(round.verification.Utils.procType), (extractType(arg)))
        case TypeRef(_, TypeName("ProcessID"), List()) =>
          round.verification.Utils.procType
        case other =>
          if (other.toString == "round.ProcessID") {
            round.verification.Utils.procType
          } else {
            //TODO
            //println("extractType:\n  " + other + "\n  " + showRaw(other))
            Wildcard
          }
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
    case Ident(TypeName(tn)) => UnInterpreted(tn)
    case _ => sys.error("TODO extractType from Tree: " + showRaw(t))
  }
  
  def extractTypeVar(t: Tree): round.formula.TypeVariable = extractType(t) match{
    case tv @ TypeVariable(_) => tv
    case other => c.abort(t.pos, "cannot extract TypeVariable from: " + other)
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
    val n = extractVarFromValDef(params.head)
    val f2 = tree2Formula(body)
    val d = extractDomain(domain)
    //println("x= " + params.head + ", " + n + ", " + n.tpe)
    //println("d= " + domain + ", " + d + ", " + d.map(_.tpe) + "\n")
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
              List(fct @ Select(This(_), v))), TermName("apply")) =>
      UnInterpretedFct(round.verification.Utils.initPrefix + v.toString, Some(extractType(fct.tpe))) //TODO type parameters ?!
    case Select(
              Apply(TypeApply(Select(Select(This(_), TermName("VarHelper")), TermName("old")), List(TypeTree())),
              List(fct @ Select(This(_), v))), TermName("apply")) =>
      UnInterpretedFct(round.verification.Utils.oldPrefix + v.toString, Some(extractType(fct.tpe))) //TODO type parameters ?!
    case TypeApply(Select(Select(Ident(scala), TermName("Some")), TermName("apply")), List(tpt)) =>
      FSome
    //TODO clean that part
    case q"${fct: RefTree}.apply" =>
      //c.echo(e.pos, "(1) considering "+ e +" as an UnInterpretedFct " + showRaw(e) + " with type " + e.tpe)
      UnInterpretedFct(fct.name.toString, Some(extractType(e.tpe))) //TODO type parameters
    case q"${fct: RefTree}.$fct2" =>
      //c.echo(e.pos, "(2) considering "+ e +" as an UnInterpretedFct " + showRaw(e + " with type " + e.tpe))
      UnInterpretedFct(fct.name.toString + "_" + fct2.toString, Some(extractType(e.tpe))) //TODO type parameters
    case q"$pkg.this.$fct" =>
      //c.echo(e.pos, "(3) considering "+ e +" as an UnInterpretedFct " + showRaw(e + " with type " + e.tpe))
      UnInterpretedFct(/*pkg.name.toString + "_" +*/ fct.toString, Some(extractType(e.tpe))) //TODO type parameters
    case Ident(TermName(fct)) =>
      //c.echo(e.pos, "(4) considering "+ e +" as an UnInterpretedFct " + showRaw(e + " with type " + e.tpe))
      UnInterpretedFct(fct.toString, Some(extractType(e.tpe))) //TODO type parameters
    case _ => sys.error("extractSymbol: " + showRaw(e))
  }

  def extractValDef(e: Tree): (Variable, Formula) = e match{
    case q"$mods val $tname: $tpt = $expr" =>
      val rhs = tree2Formula(expr)
      val t = extractType(tpt.tpe)
      val v = Variable(tname.toString).setType(t)
      (v, Eq(v, rhs))
    case _ => sys.error("expected ValDef: " + showRaw(e))
  }
  
  def extractVarFromValDef(e: Tree): Variable = e match{
    case q"$mods val $tname: $tpt = $expr" =>
      val v = tname.toString
      val t = extractType(tpt.tpe)
      Variable(v).setType(t)
    case _ => sys.error("expected ValDef: " + showRaw(e))
  }

  private def knows(op: Name) = {
    val s = op.toString
    val res = InterpretedFct.knows(s) || AxiomatizedFct.knows(s)
    //println(op + " -> " + res)
    res
  }

  def mkKnown(op: Name, args: List[Tree]): Formula = {
    val s = op.toString
    if (InterpretedFct.knows(s)) {
      val i = InterpretedFct(s).get
      val args2 = args map tree2Formula
      //TODO in case of static fct, remove the first arg as it is a package/object, e.g., None
      if (i.arity == args2.size - 1) i.application(args2.tail)
      else i.application(args2)
    } else if (AxiomatizedFct.knows(s)) {
      val u = AxiomatizedFct.symbol(s).get
      val args2 = args map tree2Formula
      Application(u, args2)
    } else {
      sys.error("unknown known ?!!: " + s)
    }
  }

  //since the backend is not high order we need to generate a new AxiomatizedFct
  def mkMinMaxBy(name: String, set: Tree, v: ValDef, expr: Tree, ineq: InterpretedFct): Formula = {
    val vf = extractVarFromValDef(v)
    val t = vf.tpe
    val s = Variable("S").setType(FSet(t))
    val u = UnInterpretedFct(name, Some(FSet(t) ~> t)) //TODO type param ?
    val app = Application(u, List(s))
    val expr2 = tree2Formula(expr)
    val expr3 = FormulaUtils.mapAll({ case `vf` => app; case f => f }, expr2)
    val axioms = List( ForAll(List(vf, s), And(In(app, s), Geq(expr3, expr2))) )
    AxiomatizedFct.add(new AxiomatizedFct(u, axioms))
    val set2 = tree2Formula(set)
    Application(u, List(set2))
  }

  def tree2Formula(e: Tree): Formula = {
    val formula: Formula = e match {
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

      //our stuff
      case q"$scope.SpecHelper.BoolOps($l).==>($r)" =>
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Implies(l2,r2)
      case Apply(TypeApply(Select(Select(This(_), TermName("VarHelper")), TermName("getter")), List(TypeTree())), List(expr)) =>
        tree2Formula(expr)
      case q"$pkg.this.broadcast($expr)" =>
        val payload = tree2Formula(expr)
        val tpe = Product(List(payload.tpe, round.verification.Utils.procType))
        val msg = Variable(Namer("__msg")).setType(tpe)
        val fst = Fst(msg)
        Comprehension(List(msg), Eq(fst, payload)).setType(FSet(tpe))

      //interpreted/known symbols
      case Apply(Select(l, op), args) if knows(op) => 
        mkKnown(op, l :: args)
      case Apply(TypeApply(Select(l, op), _), args) if knows(op) => 
        mkKnown(op, l :: args)
      case Select(l, op) if knows(op) =>
        mkKnown(op, List(l))

      // set operation, comparison, cardinality
      case q"scala.this.Predef.Set.apply[$tpt](..$args)" =>
        val t = extractType(tpt)
        val v = Variable(c.freshName("v")).setType(t)
        val args2 = args map tree2Formula
        val f = args2.foldLeft(False(): Formula)( (acc, x) => Or(acc, Eq(v, x)))
        Comprehension(List(v), f).setType(FSet(t))
      case q"$set.maxBy[$tpt]( $v => $expr )($ordering)" =>
        mkMinMaxBy(c.freshName("maxBy"), set, v, expr, Geq)
      case q"$set.minBy[$tpt]( $v => $expr )($ordering)" =>
        mkMinMaxBy(c.freshName("minBy"), set, v, expr, Leq)

      // tuples
      case q"scala.Tuple2.apply[..$tpt](..$args)" =>
        val tpt2 = tpt.map(extractType)
        val args2 = args.map(tree2Formula)
        Application(Tuple, args2).setType(Product(tpt2))
      case q"scala.this.Predef.ArrowAssoc[$tpt1]($l).->[$tpt2]($r)" =>
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        val tl = extractType(tpt1)
        val tr = extractType(tpt2)
        Application(Tuple, List(l2, r2)).setType(Product(List(tl, tr)))
      case q"scala.Tuple3.apply[..$tpt](..$args)" =>
        val tpt2 = tpt.map(extractType)
        val args2 = args.map(tree2Formula)
        Application(Tuple, args2).setType(Product(tpt2))

      // options
      case q"scala.Some.apply[$tpt]($expr)" =>
        val tpt2 = extractType(tpt) 
        val expr2 = tree2Formula(expr)
        FSome(expr2).setType(FOption(tpt2))
      case q"scala.None" =>
        Application(FNone, Nil)

      //not caught before, ignore ...
      case TypeApply(e, _) =>
        tree2Formula(e)
      
      case Typed(e, _) =>
        tree2Formula(e)
     
      //(un)interpreted fct
      case t @ q"$expr(..$args)" =>
        //println("t = " + showRaw(t))
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
     
      //literals and vars
      case Literal(Constant(v: Boolean)) => round.formula.Literal(v)
      case Literal(Constant(v: scala.Int)) => round.formula.Literal(v)
      case q"${ref: RefTree}" =>
        val n = ref.name.toString
        Variable(n).setType(extractType(ref.tpe))

      //defs
      case Block(defs, f) =>
        val f2 = tree2Formula(f)
        val (vs, d) = (defs map extractValDef).unzip
        Exists(vs, d.foldLeft(f2)((x, y) => And(x, y)))

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
        if (currRet.isDefined) {
          makeConstraints(Assign(currRet.get, term))
        } else {
          tree2Formula(term)
        }
     
      case term: RefTree =>
        if (currRet.isDefined) {
          makeConstraints(Assign(currRet.get, term))
        } else {
          tree2Formula(term)
        }
      
      case Match(selector, cases) =>
        c.abort(body.pos, "pattern matching an algebraic datatypes not yet supported")
      //for loops
      case LabelDef(name, params, rhs) =>
        c.abort(body.pos, "while loop not yet supported")
      //case Apply(id @ Ident(_), paramss) if id.symbol.isLabel =>
      //  c.abort(c.enclosingPosition, "loop not yet supported")
      case Try(block, catches, finalizer) =>
        c.abort(body.pos, "try/catch yet supported")
      case Throw(expr) =>
        c.abort(body.pos, "throwing exception not yet supported")
     
      case other =>
        c.abort(body.pos, "makeConstraints, did not expect: " + other)
    }
  }

}
