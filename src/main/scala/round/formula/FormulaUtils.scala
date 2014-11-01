package round.formula

import dzufferey.utils.Namer

object FormulaUtils {

  def alpha(map: Map[Variable, Variable], f: Formula): Formula = {
    val a = new Alpha(map)
    a.transform(f)
  }

  def alphaAll(map: Map[Variable, Variable], f: Formula): Formula = {
    val a = new AlphaAll(map)
    a.transform(f)
  }

  def map(fct: Formula => Formula, f: Formula): Formula = {
    val m = new Mapper(fct)
    m.transform(f)
  }
  
  /* Requires that bound variables are bound to variables (otherwise fails) */
  def mapWithScope(fct: (Set[Variable], Formula) => Formula, f: Formula): Formula = {
    val m = new MapperWithScope(fct)
    m.transform(f)
  }

  /* Requires that bound variables are bound to variables (otherwise fails) */
  def mapAll(fct: Formula => Formula, f: Formula): Formula = {
    val m = new MapperAll(fct)
    m.transform(f)
  }
  
  def mapSymbol(fct: Symbol => Symbol, f: Formula): Formula = {
    val m = new MapperSym(fct)
    m.transform(f)
  }

  /** Rename all free variables that appears in the formula. */
  def renameFreeVar(f: Formula): (Formula, Map[Variable, Variable]) = {
    val free = f.freeVariables
    val mapping = (Map[Variable, Variable]() /: free)( (acc, v) => acc + (v -> Copier.Variable(v, Namer(v.name))))
    (alpha(mapping, f), mapping)
  }

  protected def flatten1(i: InterpretedFct, f: Formula): List[Formula] = f match {
    case Application(`i`, lst) => lst.flatMap(flatten1(i, _))
    case Application(other, lst) => List(Copier.Application(f, other, lst map flatten))
    case Binding(b, v, f) => List(Copier.Binding(f, b, v, flatten(f)))
    case other => List(other)
  }

  def flatten(f: Formula): Formula = f match {
    case Application(Plus, lst) => Copier.Application(f, Plus, lst.flatMap(flatten1(Plus, _)))
    case Application(Times, lst) => Copier.Application(f, Times, lst.flatMap(flatten1(Times, _)))
    case Application(Union, lst) => Copier.Application(f, Union, lst.flatMap(flatten1(Union, _)))
    case Application(Intersection, lst) => Copier.Application(f, Intersection, lst.flatMap(flatten1(Intersection, _)))
    case Application(And, lst) => Copier.Application(f, And, lst.flatMap(flatten1(And, _)))
    case Application(Or, lst) => Copier.Application(f, Or, lst.flatMap(flatten1(Or, _)))
    case Application(other, lst) => Copier.Application(f, other, lst map flatten)
    case Binding(b, v, f) =>Copier. Binding(f, b, v, flatten(f))
    case other => other
  }

  def purify(f: Formula): Formula = {
    val p = new Purifier
    assert(p.collected.isEmpty, "purifying should be called on formula which is boolean at the top level.")
    p.transform(f)
  }

  def getConjuncts(f: Formula): List[Formula] = f match {
    case And(lst) => lst.flatMap(getConjuncts)
    case other => List(other)
  }
  
  def getDisjuncts(f: Formula): List[Formula] = f match {
    case Or(lst) => lst.flatMap(getDisjuncts)
    case other => List(other)
  }

  def restoreQuantifierPrefix(prefix: List[(BindingType,List[Variable])], qf: Formula): Formula = {
    prefix.foldRight(qf)( ( p, acc) => Binding(p._1, p._2, acc).setType(Bool) )
  }

  /* assumes PNF */
  def getQuantifierPrefix(f: Formula): (List[(BindingType,List[Variable])], Formula) = f match {
    case ForAll(vs, f2) =>
      val (prf, f3) = getQuantifierPrefix(f2)
      ((ForAll -> vs) :: prf, f3)
    case Exists(vs, f2) =>
      val (prf, f3) = getQuantifierPrefix(f2)
      ((Exists -> vs) :: prf, f3)
    case other =>
      (Nil, other)
  }
  
  def existentiallyBound(f: Formula): Set[Variable] = {
    def process(acc: Set[Variable], f: Formula) = f match {
      case Exists(vs, _) => acc ++ vs
      case _ => acc
    }
    collect(Set[Variable](), process, f)
  }

  def universallyBound(f: Formula): Set[Variable] = {
    def process(acc: Set[Variable], f: Formula) = f match {
      case ForAll(vs, _) => acc ++ vs
      case _ => acc
    }
    collect(Set[Variable](), process, f)
  }
  
  
  def traverse(fct: Formula => Unit, f: Formula) {
    val traverser = new Traverser {
      override def traverse(f: Formula) = {
        super.traverse(f)
        fct(f)
      }
    }
    traverser.traverse(f)
  }


  def collect[T](init: T, fct: (T, Formula) => T, f: Formula): T = {
    var acc = init
    val traverser = new Traverser {
      override def traverse(f: Formula) = {
        super.traverse(f)
        acc = fct(acc, f)
      }
    }
    traverser.traverse(f)
    acc
  }

  def collectWithScope[T](init: T, fct: (T, Set[Variable], Formula) => T, f: Formula): T = {
    var acc = init
    val traverser = new TraverserWithScope {
      override def traverse(bound: Set[Variable], f: Formula) = {
        super.traverse(bound, f)
        acc = fct(acc, bound, f)
      }
    }
    traverser.traverse(f)
    acc
  }

  def collectTypes(f: Formula): Set[Type] =
    collect(Set[Type](), (acc: Set[Type], f: Formula) => acc + f.tpe, f)

  def collectSymbols(f: Formula): Set[Symbol] = {
    def process(acc: Set[Symbol], f: Formula) = f match {
      case Application(s, _) => acc + s
      case _ => acc
    }
    collect(Set[Symbol](), process, f)
  }

  def typeParams(app: Application): List[Type] = app.fct match {
    case Tuple => app.args.map(_.tpe)
    case Fst | Snd | Trd =>
      app.args.head.tpe match {
        case Product(tps) => tps
        case other => sys.error("typeParams expected product type: " + other + " in " + app)
      }
    case Eq | Neq | And | Or | Plus | Times => //skip those: overloaded in smtlib of flattened
      Nil
    case normal =>
      val params = normal.typeParams
      val concreteType = Function(app.args.map(_.tpe), app.tpe)
      val subst = Typer.unify(normal.typeWithParams, concreteType)
      subst match {
        case Some(s) =>
          //println("typeWithParams: " + app.fct + ", " + concreteType + ", " + normal.typeWithParams + ", " + subst)
          params.map(s)
        case None => sys.error("FormulaUtils.typeWithParams, cannot unify: " + normal.typeWithParams + ", " + concreteType)
      }
  }
  
  def collectSymbolsWithParams(f: Formula): Set[(Symbol, List[Type])] = {
    def process(acc: Set[(Symbol, List[Type])], f: Formula) = f match {
      case app @ Application(s, _) => acc + (s -> typeParams(app))
      case _ => acc
    }
    collect(Set[(Symbol, List[Type])](), process, f)
  }

  def collectGroundTerms(f: Formula): Set[Formula] = {
    def collect(f: Formula, bound: Set[Variable]): (Set[Formula], Boolean) = f match {
      case Binding(_, vs, f) => (collect(f, bound ++ vs)._1, false)
      case a @ Application(fct, args) =>
        val (acc, ground) = args.foldLeft( (Set[Formula](),true))( (acc,f) => {
          val (x,y) = collect(f, bound)
          (acc._1 ++ x, acc._2 && y)
        })
        if (ground && a.tpe != Bool) (acc + a, true)
        else (acc, false)
      case l @ Literal(_) =>
        (Set(l), true)
      case v @ Variable(_) =>
        if (bound(v)) (Set(), false)
        else (Set(v), true)
    }
    collect(f, Set())._1
  }

}

