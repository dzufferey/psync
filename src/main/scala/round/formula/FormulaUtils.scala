package round.formula

import dzufferey.utils.Namer

object FormulaUtils {

  implicit object BindingTypeOrdering extends Ordering[BindingType] {
    def compare(a: BindingType, b: BindingType) = {
      if (a == b) 0
      else (a,b) match {
        case (Exists, _) => -1
        case (ForAll, Exists) => 1
        case (ForAll, _) => -1
        case (Comprehension, _) => 1
      }
    }
  }

  implicit object SymbolOrdering extends Ordering[Symbol] {
    def compare(a: Symbol, b: Symbol) = (a,b) match {
      case (a: InterpretedFct, b: InterpretedFct) => a.symbol compare b.symbol
      case (a: InterpretedFct, _) => -1
      case (a: UnInterpretedFct, b: InterpretedFct) => 1
      case (UnInterpretedFct(n1, t1, p1), UnInterpretedFct(n2, t2, p2)) =>
        val n = n1 compare n2
        assert(n != 0 || (t1 == t2 && p1 == p2), "overloading not currently supported")
        n
    }
  }

  implicit object VariableOrdering extends Ordering[Variable] {
    def compare(a: Variable, b: Variable) = a.name compare b.name
  }

  implicit object FormulaOrdering extends Ordering[Formula] {
    val lstOrdering = Ordering.Iterable(this)

    def compareLiteralContent(a: Any, b: Any): Int = (a, b) match {
      case (l1: Long, l2: Long) => l1 compare l2
      case (d1: Double, d2: Double) => d1 compare d2
      case (other1, other2) =>
        if (other1 == other2) 0
        else {
          val h1 = other1.hashCode
          val h2 = other2.hashCode
          assert(h1 != h2, "don't know how to compare " + other1 + " and " + other2)
          h1 compare h2
        }
    }

    def compare(a: Formula, b: Formula) = (a,b) match {
      case (Literal(l1), Literal(l2)) => compareLiteralContent(l1, l2)
      case (Literal(al), _) => -1
      case (Variable(_), Literal(_)) => 1
      case (Variable(v1), Variable(v2)) => v1 compare v2
      case (Variable(_), _) => -1
      case (Application(_, _), Literal(_) | Variable(_)) => 1
      case (Application(f1, arg1), Application(f2, arg2)) =>
        val f = SymbolOrdering.compare(f1, f2)
        if (f == 0) lstOrdering.compare(arg1, arg2)
        else f
      case (Application(_, _), _) => -1
      case (Binding(b1, v1, f1), Binding(b2, v2, f2)) =>
        val b = BindingTypeOrdering.compare(b1, b2)
        if (b == 0) {
          val v = lstOrdering.compare(v1, v2)
          if (v == 0) compare(f1, f2)
          else v
        } else b
      case (Binding(_,_,_), _) => 1
    }
  }
  
  //TODO use this in the normalization
  val commutative = Set[Symbol](And,Or,Eq,Neq,Plus,Times,Union,Intersection)

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
  
  def replace(from: Formula, to: Formula, f: Formula): Formula = {
    assert(from.tpe == to.tpe, "replacing " + from + " by " + to + " does not respect type: " + from.tpe + ", " + to.tpe)
    def fct(e: Formula) = if (e == from) to else e
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
  
  def mapTopDown(fct: Formula => Formula, f: Formula): Formula = {
    val m = new TopDownMapper(fct)
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
    case Binding(b, v, f) => Copier.Binding(f, b, v, flatten(f))
    case other => other
  }

  def purify(f: Formula): Formula = {
    val p = new Purifier
    assert(p.collected.isEmpty, "purifying should be called on formula which is boolean at the top level.")
    p.transform(f)
  }

  def getConjuncts(f: Formula): List[Formula] = f match {
    case And(lst @ _*) => lst.flatMap(getConjuncts).toList
    case other => List(other)
  }
  
  def getDisjuncts(f: Formula): List[Formula] = f match {
    case Or(lst @ _*) => lst.flatMap(getDisjuncts).toList
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
          //println("typeWithParams: " + app.fct + ", " + concreteType + ", " + params + ", " + subst + ", " + s)
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

  val symbolExcludedFromGroundTerm = Set[Symbol](And,Or,Not,Implies,Eq,Neq)

  def exists(p: Formula => Boolean, f: Formula): Boolean = {
    if (p(f)) true
    else f match {
      case Binding(_, _, f1) => exists(p, f1)
      case Application(_, args) => args.exists(exists(p, _))
      case _ => false
    }
  }

  /** is f2 a descendent of f1 */
  def contains(f1: Formula, f2: Formula): Boolean = exists( x => x == f2, f1)

  def collectGroundTerms(f: Formula): Set[Formula] = {
    def collect(f: Formula, bound: Set[Variable]): (Set[Formula], Boolean) = f match {
      case Binding(_, vs, f) => (collect(f, bound ++ vs)._1, false)
      case a @ Application(fct, args) =>
        val (acc, ground) = args.foldLeft( (Set[Formula](),true))( (acc,f) => {
          val (x,y) = collect(f, bound)
          (acc._1 ++ x, acc._2 && y)
        })
        if (ground && !symbolExcludedFromGroundTerm(fct)) (acc + a, true)
        else (acc, false)
      case l @ Literal(_) =>
        (Set(l), true)
      case v @ Variable(_) =>
        if (bound(v)) (Set(), false)
        else (Set(v), true)
    }
    collect(f, Set())._1
  }
  
  def collectVariables(f: Formula): Set[Variable] = {
    def process(acc: Set[Variable], f: Formula): Set[Variable] = f match {
      case Binding(_, vs, f) => acc ++ vs
      case v: Variable => acc + v
      case _ => acc
    }
    collect(Set[Variable](), process, f)
  }

}

