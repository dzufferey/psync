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

  def getConjunts(f: Formula): List[Formula] = f match {
    case And(lst) => lst.flatMap(getConjunts)
    case other => List(other)
  }

  def collectTypes(f: Formula): Set[Type] = {
    var tpes = Set[Type]()
    val traverser = new Traverser {
      override def traverse(f: Formula) = {
        super.traverse(f)
        tpes = tpes + f.tpe 
      }
    }
    traverser.traverse(f)
    tpes
  }

  def collectSymbols(f: Formula): Set[Symbol] = {
    var sym = Set[Symbol]()
    val traverser = new Traverser {
      override def traverse(f: Formula) = {
        super.traverse(f)
        f match {
          case Application(s, _) => sym = sym + s
          case _ => ()
        }
      }
    }
    traverser.traverse(f)
    sym
  }

  def typeParams(app: Application): List[Type] = app.fct match {
    case Tuple | Fst | Snd | Trd =>
      sys.error("TODO typeParams for Product type: " + app)
    case Eq | Neq | And | Or | Plus | Times => //skip those: overloaded in smtlib of flattened
      Nil
    case normal =>
      val params = normal.typeParams
      val concreteType = Function(app.args.map(_.tpe), app.tpe)
      val subst = Typer.unify(normal.typeWithParams, concreteType)
      subst match {
        case Some(s) => params.map(s)
        case None => sys.error("FormulaUtils.typeWithParams, cannot unify: " + normal.typeWithParams + ", " + concreteType)
      }
  }
  
  def collectSymbolsWithParams(f: Formula): Set[(Symbol, List[Type])] = {
    var sym = Set[(Symbol, List[Type])]()
    val traverser = new Traverser {
      override def traverse(f: Formula) = {
        super.traverse(f)
        f match {
          case app @ Application(s, _) => sym = sym + (s -> typeParams(app))
          case _ => ()
        }
      }
    }
    traverser.traverse(f)
    sym
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

