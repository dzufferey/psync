package psync.macros

import psync.formula._
import psync.verification._
import dzufferey.utils.Namer

trait FormulaExtractor {
  self: Impl =>
  import c.universe._
  
  def extractTypeVar(t: Tree): psync.formula.TypeVariable = extractType(t) match{
    case tv @ TypeVariable(_) => tv
    case other => c.abort(t.pos, "cannot extract TypeVariable from: " + other)
  }

  def extractDomain(e: Tree): Option[Formula] = {
    if (e.tpe.typeConstructor.toString contains "Domain") {
      None
    } else {
      Some(tree2Formula(e))
    }
  }

  def makeBindingSet(b: BindingType, domain: Tree, params: List[ValDef], body: Tree): Binding = {
    assert(params.length == 1)
    val n = extractVarFromValDef(params.head)
    val f2 = tree2Formula(body)
    val d = extractDomain(domain)
    val res = b match {
      case Exists | Comprehension =>
        Binding(b, List(n), d.map( d => And(In(n,d),f2)).getOrElse(f2))
      case ForAll =>
        Binding(b, List(n), d.map( d => Implies(In(n,d),f2)).getOrElse(f2))
    }
    res
  }

  def makeBindingMap(b: BindingType, domain: Tree, params: List[ValDef], body: Tree): Formula = {
    assert(params.length == 1)
    body match {
      case q"$id match { case ($k, $v) => $expr }" =>
        val d  = extractDomain(domain).get
        val kv = extractVarFromPattern(k)
        val vv = extractVarFromPattern(v)
        val predRaw = tree2Formula(expr)
        val pred = FormulaUtils.replace(vv, LookUp(d, kv), predRaw)
        b match {
          case Exists => Exists(List(kv), And(In(kv, KeySet(d)), pred))
          case ForAll => ForAll(List(kv), Implies(In(kv, KeySet(d)), pred))
          case Comprehension =>
            val v = Variable(c.freshName("filteredMap"))
            addCstr(Eq(KeySet(v), Comprehension(List(kv), And(In(kv, KeySet(d)), pred)) ))
            addCstr(ForAll(List(kv), Eq(LookUp(d, kv), LookUp(v, kv))))
            v
        }
      case _ =>
        c.warning(domain.pos, "makeBindingMap: " + b + ", " + domain + " -> " + params + " => " + body)
        val v = Variable(c.freshName("dummy"))
        b match {
          case Exists | ForAll => v.setType(Bool)
          case Comprehension => v.setType(typeOfTree(domain))
        }
    }
  }

  def makeBinding(b: BindingType, domain: Tree, params: List[ValDef], body: Tree): Formula = typeOfTree(domain) match {
    case FSet(_) =>
      makeBindingSet(b, domain, params, body)
    case FMap(_, _) =>
      makeBindingMap(b, domain, params, body)
    case other =>
      c.abort(domain.pos, "makeBinding: unknown domain type → " + other + " for " + domain + ", " + domain.tpe + ", " + domain.symbol.typeSignature)
  }

  /*
  def mkInit(v: Name, t: Type): psync.formula.Symbol = {
    UnInterpretedFct(psync.verification.Utils.initPrefix + v.toString, Some(extractType(t))) //TODO type parameters ?!
  }
  def mkInit(v: Tree, t: Type): psync.formula.Symbol = v match {
    case q"$pkg.this.$name" => mkInit(name, t)
    case other => sys.error("mkInit: " + showRaw(v))
  }
  
  def mkOld(v: Name, t: Type): psync.formula.Symbol = {
    UnInterpretedFct(psync.verification.Utils.oldPrefix + v.toString, Some(extractType(t))) //TODO type parameters ?!
  }
  def mkOld(v: Tree, t: Type): psync.formula.Symbol = v match {
    case q"$pkg.this.$name" => mkOld(name, t)
    case other => sys.error("mkOld: " + showRaw(v))
  }
  */

  def extractSymbol(e: Tree): psync.formula.Symbol = e match {
    /*
    case Select(
              Apply(TypeApply(Select(Select(This(_), TermName("VarHelper")), TermName("init")), List(TypeTree())),
              List(fct @ Select(This(_), v))), TermName("apply")) =>
      mkInit(v, fct.tpe)
    case Select(
              Apply(TypeApply(Select(Select(This(_), TermName("VarHelper")), TermName("old")), List(TypeTree())),
              List(fct @ Select(This(_), v))), TermName("apply")) =>
      mkOld(v, fct.tpe)
    */
    case TypeApply(Select(Select(Ident(scala), TermName("Some")), TermName("apply")), List(tpt)) =>
      FSome
    //TODO clean that part
    case q"${fct: RefTree}.apply" =>
      val tpe = extractType(e.tpe)
      //c.echo(e.pos, "(1) "+ e +" as an UnInterpretedFct with type " + e.tpe + ", " + tpe)
      UnInterpretedFct(fct.name.toString, Some(tpe)) //TODO type parameters
    case q"${fct: RefTree}.$fct2" =>
      val tpe = extractType(e.tpe)
      //c.echo(e.pos, "(2) "+ e +" as an UnInterpretedFct with type " + e.tpe + ", " + tpe)
      UnInterpretedFct(fct.name.toString + "_" + fct2.toString, Some(tpe)) //TODO type parameters
    case q"$pkg.this.$fct" =>
      val tpe = extractType(e.tpe)
      //c.echo(e.pos, "(3) "+ e +" as an UnInterpretedFct with type " + e.tpe + ", " + tpe)
      UnInterpretedFct(/*pkg.name.toString + "_" +*/ fct.toString, Some(tpe)) //TODO type parameters
    case Ident(TermName(fct)) =>
      val tpe = extractType(e.tpe)
      //c.echo(e.pos, "(4) "+ e +" as an UnInterpretedFct with type " + e.tpe + "," + tpe)
      UnInterpretedFct(fct.toString, Some(tpe)) //TODO type parameters
    case _ => sys.error("extractSymbol: " + showRaw(e))
  }

  def extractVarFromValDef(e: Tree): Variable = e match {
    case q"$mods val $tname: $tpt = $expr" =>
      val v = tname.toString
      val t = extractType(tpt.tpe)
      Variable(v).setType(t)
    case _ => sys.error("expected ValDef: " + showRaw(e))
  }

  def extractVarFromPattern(e: Tree): Variable = e match {
    case Bind(TermName(n), Ident(termNames.WILDCARD)) => Variable(n).setType(typeOfTree(e))
    case other => sys.error("extractVarFromPattern, did not expect: " + other)
  }

  private def knows(op: Name, args: List[Tree], pos: Position) = {
    val s = op.toString
    if (InterpretedFct.knows(s)) {
      val tpes = args.map(typeOfTree)
      val res = resolveOverloading(s, tpes, pos)
      res.isDefined
    } else {
      AxiomatizedFct.knows(s)
    }
  }
  
  private def resolveOverloading(s: String, args: List[psync.formula.Type], pos: Position): Option[InterpretedFct] = {
    val is = InterpretedFct(s)
    val candidates = is.flatMap{ i => 
      //in case of static fct, remove the first arg as it is a package/object, e.g., None
      val args2 = if (i.arity == args.size - 1) args.tail else args
      //check if it can be unified
      val t = i.tpe(args2.length)
      val ret = Type.freshTypeVar
      val unifier = Typer.unify(t, psync.formula.Function(args2, ret))
      if (unifier.isDefined) Some(i)
      else None
    }
    if (candidates.size > 1) {
      c.warning(pos, "cannot resolve overloading for " + s + ":\n  " +
                     args.mkString(", ") + "\n  " +
                     candidates.mkString(", "))
    }
    candidates.headOption
  }
  
  def mkKnown(op: Name, args: List[Tree], pos: Position): Formula = {
    val s = op.toString
    if (InterpretedFct.knows(s)) {
      val args2 = args map tree2Formula
      resolveOverloading(s, args2.map(_.tpe), pos) match {
        case Some(i) =>
          val args3 = if (i.arity == args2.size - 1) args2.tail else args2
          i(args3:_*)
        case None =>
          sys.error("known but cannot find symbol: " + args.mkString(op.toString+"(",", ",") -> ") + args2.map(_.tpe))
      }
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
    val u = UnInterpretedFct(name, Some(FSet(t) ~> t)) //TODO type param ?
    val set2 = tree2Formula(set)
    val res = Application(u, List(set2)).setType(t)
    val expr2 = tree2Formula(expr)
    val expr3 = FormulaUtils.mapAll({ case `vf` => res; case f => f }, expr2)
    val cstr = ForAll(List(vf), Implies(In(vf, set2), ineq(expr3, expr2))) 
    addCstr( cstr )
    //println(name + " -> " + Some(FSet(t) ~> t))
    res
  }

  protected var auxCstr: List[Formula] = Nil
  def addCstr(f: Formula) {
    auxCstr = f :: auxCstr
  }
  def getCstr = {
    val res = auxCstr
    auxCstr = Nil
    res
  }

  def removeProcTypeArg(f: Formula): Formula = f.tpe match {
    case psync.formula.Function(List(t), ret) if t == psync.verification.Utils.procType => f.setType(ret)
    case _ => f
  }

  def tree2Formula(e: Tree): Formula = {
    val formula: Formula = e match {
      // quantifiers, comprehensions
      case q"$domain.forall( ..$xs => $f )" => makeBinding(ForAll, domain, xs, f)
      case q"$domain.exists( ..$xs => $f )" => makeBinding(Exists, domain, xs, f)
      case q"$domain.filter( ..$xs => $f )" => makeBinding(Comprehension, domain, xs, f)
      case q"$domain.map[$tpt1,$tpt2]( $x => $f )(immutable.this.Set.canBuildFrom[$tpt3])" =>
        // { y | x ∈ domain ∧ y = f(x) }
        val y = Variable(c.freshName("y")).setType(typeOfTree(tpt1))
        val fCstr = makeConstraints(f, y, y)
        val vx = extractVarFromValDef(x)
        val witness = Application(UnInterpretedFct(c.freshName("witness"), Some(y.tpe ~> vx.tpe)), List(y)).setType(vx.tpe)
        val fCstr2 = FormulaUtils.replace(vx, witness, fCstr)
        val dCstr = In(witness, tree2Formula(domain))
        Comprehension(List(y), And(dCstr, fCstr2))
      case q"$domain.map[$tpt1,$tpt2]( $x => $f )(immutable.this.Map.canBuildFrom[$tpt3,$tpt4])" =>
        extractType(tpt1) match {
          case Product(List(tK, tV)) =>
            f match {
              case q"$id match { case ($k, $v) => $expr }" =>
                val id = Variable(c.freshName("mappedMap")).setType(FMap(tK, tV))
                val d = tree2Formula(domain)
                val kv = extractVarFromPattern(k)
                val vv = extractVarFromPattern(v)
                val retVar = Variable(c.freshName("retV")).setType(Product(List(tK,tV)))
                val body0 = makeConstraints(expr, retVar, retVar)
                addCstr(Eq(KeySet(id), KeySet(d)))
                body0 match {
                  case Eq(`retVar`, Tuple(`kv`, value)) =>
                    addCstr(ForAll(List(kv), Eq(LookUp(id, kv), FormulaUtils.replace(vv, LookUp(d, kv), value))))
                  case other =>
                    c.warning(expr.pos, "body too complicated, leaving it unconstrained: " + other)
                }
                id
              case other =>
                c.abort(domain.pos, "Map.map, expected pair function. found: " + other)
            }
          case other =>
            c.abort(domain.pos, "Map.map, expected pairs of type. found: " + other)
        }
      
      case q"$scope.SpecHelper.init[$tpt]($expr)" =>
        tree2Formula(expr) match {
          case a @ Application(UnInterpretedFct(f, t, p), args) =>
            val f2 = UnInterpretedFct(psync.verification.Utils.initPrefix + f, t, p)
            Application(f2, args).setType(a.tpe)
          case other => c.abort(expr.pos, "expected var access, found: " + other)
        }
      case q"$scope.SpecHelper.old[$tpt]($expr)" =>
        tree2Formula(expr) match {
          case a @ Application(UnInterpretedFct(f, t, p), args) =>
            val f2 = UnInterpretedFct(psync.verification.Utils.oldPrefix + f, t, p)
            Application(f2, args).setType(a.tpe)
          case other => c.abort(expr.pos, "expected var access, found: " + other)
        }

      //our stuff
      case q"$scope.SpecHelper.BoolOps($l).==>($r)" =>
        val l2 = tree2Formula(l)
        val r2 = tree2Formula(r)
        Implies(l2,r2)
      case q"$pkg.this.broadcast[$tpt]($expr)" =>
        val payload = tree2Formula(expr)
        val tpe = FMap(psync.verification.Utils.procType, payload.tpe)
        val mapName = Variable(c.freshName("bcast")).setType(tpe)
        val p = Variable(c.freshName("p")).setType(psync.verification.Utils.procType)
        addCstr(Eq(KeySet(mapName), Comprehension(List(p), True())))
        addCstr(ForAll(List(p), Eq(LookUp(mapName, p), payload)))
        mapName

      // set construction
      case q"scala.this.Predef.Set.empty[$tpt]" =>
        val t = extractType(tpt)
        val v = Variable(c.freshName("v")).setType(t)
        Comprehension(List(v), False()).setType(FSet(t))
      case q"scala.this.Predef.Set.apply[$tpt](..$args)" =>
        val t = extractType(tpt)
        val v = Variable(c.freshName("v")).setType(t)
        val args2 = args map tree2Formula
        val f = Or(args2.map(Eq(v,_)):_*)
        Comprehension(List(v), f).setType(FSet(t))
      
      case q"scala.this.Predef.Map.empty[$t1,$t2]" =>
        val t = typeOfTree(e)
        val v = Variable(c.freshName("emptyMap")).setType(t)
        val elt = Variable(c.freshName("v")).setType(extractType(t1))
        addCstr(Eq(KeySet(v), Comprehension(List(elt), False())))
        // LookUp is unconstrained
        v
      case q"scala.this.Predef.Map.apply[$t1,$t2](..$args)" =>
        val t = typeOfTree(e)
        val m = Variable(c.freshName("applyMap")).setType(t)
        val elt = Variable(c.freshName("v")).setType(extractType(t1))
        val args2 = args map tree2Formula
        val (keys,values) = args2.map( x => x match {
          case Tuple(k, v) =>  k -> v
          case other => Fst(other) -> Snd(other)
        }).unzip
        // LookUp
        (keys zip values) foreach { case (k, v) => addCstr(Eq(LookUp(m, k), v)) }
        // KeySet
        val elts = Or(keys.map( Eq(elt, _) ):_*)
        addCstr(Eq(KeySet(m), Comprehension(List(elt), elts )))
        //
        m

      // set operation, comparison, cardinality
      //TODO generalize to Map
      //TODO more general support for ordering (non integer type)
      case q"$set.maxBy[$tpt]( $v => $expr )($ordering)" =>
        mkMinMaxBy(c.freshName("maxBy"), set, v, expr, Geq)
      case q"$set.minBy[$tpt]( $v => $expr )($ordering)" =>
        mkMinMaxBy(c.freshName("minBy"), set, v, expr, Leq)
      case q"$set.max($ordering)" =>
        val s = tree2Formula(set)
        val f = UnInterpretedFct(c.freshName("max"), Some(FSet(Int) ~> Int), Nil)
        val res = Application(f, List(s)).setType(Int)
        val v = Variable(c.freshName("v")).setType(Int)
        addCstr( In(res, s) )
        addCstr( ForAll(List(v), Implies(In(v, s), Leq(v, res)) ) )
        res
      case q"$set.min($ordering)" =>
        val s = tree2Formula(set)
        val f = UnInterpretedFct(c.freshName("min"), Some(FSet(Int) ~> Int), Nil)
        val res = Application(f, List(s)).setType(Int)
        val v = Variable(c.freshName("v")).setType(Int)
        addCstr( In(res, s) )
        addCstr( ForAll(List(v), Implies(In(v, s), Leq(res, v)) ) )
        res
      case q"$set.head" =>
        val s = tree2Formula(set)
        val t = extractType(e.tpe)
        val st = s.tpe match {
          case Wildcard => FSet(t)
          case other => other
        }
        val h = UnInterpretedFct(c.freshName("head"), Some(st ~> t), Nil)
        val res = Application(h, List(s)).setType(t)
        addCstr( In(res, s) )
        res
      case q"$set.find( $v => $expr )" =>
        val t = extractType(e.tpe)
        c.warning(e.pos, "TODO formula for $set.find")
        FNone().setType(t)

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

      //Time
      case q"$expr1 / $expr2" if typeOfTree(expr1) == psync.verification.Utils.timeType =>
        val t = tree2Formula(expr1)
        val n = tree2Formula(expr2)
        psync.logic.ReduceTime.fromInt(Divides(psync.logic.ReduceTime.toInt(t), n))
      case q"psync.Time.fromInt($expr)" =>
        psync.logic.ReduceTime.fromInt(tree2Formula(expr))
      case q"new psync.Time($expr)" =>
        psync.logic.ReduceTime.fromInt(tree2Formula(expr))
      case q"psync.Time.toInt($expr)" if typeOfTree(expr) == psync.verification.Utils.timeType =>
        psync.logic.ReduceTime.toInt(tree2Formula(expr))
      case q"$expr.toInt" if typeOfTree(expr) == psync.verification.Utils.timeType =>
        psync.logic.ReduceTime.toInt(tree2Formula(expr))

      //init and old
      /*
      case q"$scope.VarHelper.init[$tpt]($expr)" =>
        mkInit(expr, tpt.tpe)(tree2Formula(expr))
      case q"$scope.VarHelper.old[$tpt]($expr)" =>
        mkOld(expr, tpt.tpe)(tree2Formula(expr))
      */
     
      //interpreted/known symbols
      case Apply(s @ Select(l, op), args) if knows(op, l :: args, s.pos) => mkKnown(op, l :: args, s.pos)
      case Apply(TypeApply(s @ Select(l, op), _), args) if knows(op, l :: args, s.pos) => mkKnown(op, l :: args, s.pos)
      case s @ Select(l, op) if knows(op, List(l), s.pos) => mkKnown(op, List(l), s.pos)

      //(un)interpreted fct
      case t @ q"$expr(..$args)" =>
        //println("uninterpreted: " + expr + ": " + expr.tpe + " on " + args + ": " + args.map(_.tpe))
        val fct = extractSymbol(expr)
        val args2 = args map tree2Formula
        Application(fct, args2)
      
      case fld @ q"$pkg.this.$expr" => //TODO does not seems right ...
        val n = expr.toString
        Variable(n).setType(extractType(fld.tpe))

      case q"$expr.$member" =>
        //println("e = " + showRaw(e))
        val fct: psync.formula.Symbol = UnInterpretedFct(member.toString) //TODO type
        val args = List(tree2Formula(expr))
        Application(fct, args)
     
      //literals and vars
      case Literal(Constant(v: Boolean)) => psync.formula.Literal(v)
      case Literal(Constant(v: scala.Int)) => psync.formula.Literal(v)
      case Literal(Constant(v: scala.Long)) => psync.formula.Literal(v)
      case Literal(Constant(v: scala.Short)) => psync.formula.Literal(v)
      case Literal(Constant(v: scala.Byte)) => psync.formula.Literal(v)
      case q"${ref: RefTree}" =>
        val n = ref.name.toString
        val t = typeOfTree(ref)
        Variable(n).setType(t)

      //defs
      case Block(defs, f) =>
        val f2 = tree2Formula(f)
        val (vs, d) = defs.map( x => makeConstraints(x) match {
          case e @ Eq(v @ Variable(_), _) => (v -> e)
          case other => sys.error("expected Eq, found: " + other)
        }).unzip
        Exists(vs, And( (d ::: List(f2)) :_*))

      case Literal(Constant(())) => UnitLit()

      case EmptyTree => UnitLit()

      case other => sys.error("did not expect:\n" + other + "\n" + showRaw(other))
    }

    formula.tpe match {
      case Wildcard =>
        formula.setType(typeOfTree(e))
      case _ => formula
    }
  }
  
  def getConstraints(t: Tree): Formula = {
    val oldCstr = getCstr
    val c1 = tree2Formula(t)
    val c2 = getCstr
    auxCstr = oldCstr
    if (c2.isEmpty) c1
    else And( c1::c2 :_*)
  }
  
  /* constraints for a loop-free block in SSA. */
  def makeConstraints(body: Tree): Formula =
  {
    val oldCstr = getCstr
    val c1 = makeConstraints1(body, None, None)
    val c2 = getCstr
    auxCstr = oldCstr
    if (c2.isEmpty) c1
    else And( c1::c2 :_*)
  }

  /* constraints for a loop-free block in SSA. */
  def makeConstraints(
      body: Tree,
      currRet: Tree,
      globalRet: Tree
    ): Formula =
  {
    val oldCstr = getCstr
    val cr = tree2Formula(currRet)
    val gr = tree2Formula(globalRet)
    val c1 = makeConstraints1(body, Some(cr), Some(gr))
    val c2 = getCstr
    auxCstr = oldCstr
    if (c2.isEmpty) c1
    else And( c1::c2 :_*)
  }
  
  /* constraints for a loop-free block in SSA. */
  def makeConstraints(
      body: Tree,
      currRet: Formula,
      globalRet: Formula
    ): Formula =
  {
    val oldCstr = getCstr
    val c1 = makeConstraints1(body, Some(currRet), Some(globalRet))
    val c2 = getCstr
    auxCstr = oldCstr
    if (c2.isEmpty) c1
    else And( c1::c2 :_*)
  }
  
  def makeConstraints1(
      body: Tree,
      currRet: Option[Formula] = None,
      globalRet: Option[Formula] = None
    ): Formula =
  {
    body match {
     
      case If(cond, thenp, elsep) =>
        //TODO inline cond or not in the result (less readable, might work better?)
        val id = c.freshName("cond")
        val cvar = Variable(id).setType(Bool)
        val condCstr = makeConstraints1(cond, Some(cvar), None)
        val thenCstr = makeConstraints1(thenp, currRet, globalRet)
        val elseCstr = makeConstraints1(elsep, currRet, globalRet)
        And(condCstr, Or(And(cvar, thenCstr), And(Not(cvar), elseCstr)))
     
      case Block(stats, expr) =>
        val statsCstr = stats.map(makeConstraints1(_, None, globalRet))
        val retCstr = makeConstraints1(expr, currRet, globalRet)
        statsCstr.foldRight(retCstr)(And(_,_))
     
      case Return(expr) =>
        globalRet.map( ret => Eq(ret, tree2Formula(expr))).getOrElse(True())
      
      case Typed(e, _) =>
        makeConstraints1(e, currRet, globalRet)
      
      //TODO better way of identifying setters and get the type from the setter signature ...
      case Apply(Select(This(TypeName(_)), TermName(setter)), List(rhs)) if setter.endsWith("_$eq") =>
        Eq(Variable(setter.substring(0, setter.length - 4)).setType(typeOfTree(rhs)), tree2Formula(rhs))
      case Assign(lhs, rhs) =>
        Eq(tree2Formula(lhs), tree2Formula(rhs))
     
      case vd @ ValDef(mods, name, tpe, rhs) =>
        val v = extractVarFromValDef(vd)
        makeConstraints1(rhs, Some(v), globalRet)
     
      case Literal(Constant(())) | EmptyTree =>
        True()
      
      case term: TermTree => 
        if (currRet.isDefined) {
          Eq(currRet.get, tree2Formula(term))
        } else {
          c.echo(term.pos, "makeConstraints ignoring (make sure it is not important for the verification)" + showRaw(term))
          True()
        }
     
      case term: RefTree =>
        if (currRet.isDefined) {
          Eq(currRet.get, tree2Formula(term))
        } else {
          c.echo(term.pos, "makeConstraints ignoring (make sure it is not important for the verification)" + showRaw(term))
          True()
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
