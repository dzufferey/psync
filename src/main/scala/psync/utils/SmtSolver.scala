package psync.utils

import psync.formula._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import dzufferey.smtlib.{Solver, UFLIA, Z3, CVC4MF}

object SmtSolver {

  Solver.defaultTO = 10000 // 10 seconds

  def setCmd(cmd: Array[String]) = Solver.setCmd(cmd)

  def useZ3 = setCmd(Z3.solver +: Z3.solverArg)
  def useCvc4Mf = setCmd(CVC4MF.solver +: CVC4MF.solverArg)
  
  def defaultTO(i: Int) = {
    Solver.defaultTO = i
  }

  def z3(file: Option[String], timeout: Long): Solver = {
    new Solver(UFLIA, Z3.solver, Z3.solverArg, true, true, file, timeout)
  }

  def cvc4mf(file: Option[String], timeout: Long): Solver = {
    new Solver(UFLIA, CVC4MF.solver, CVC4MF.solverArg, true, true, file, timeout)
  }

  def apply(file: Option[String], timeout: Long): Solver = {
    Solver(UFLIA, file, timeout)
  }

  def apply(file: String): Solver = {
    Solver(UFLIA, file)
  }

  def apply(): Solver = {
    Solver(UFLIA)
  }

  def convert(bt: BindingType): dzufferey.smtlib.BindingType = bt match {
    case ForAll => dzufferey.smtlib.ForAll
    case Exists => dzufferey.smtlib.Exists
    case Comprehension => Logger.logAndThrow("SmtSolver", Error, "cannot conver Comprehension")
  }

  def convert(bt: dzufferey.smtlib.BindingType): BindingType = bt match {
    case dzufferey.smtlib.ForAll => ForAll
    case dzufferey.smtlib.Exists => Exists
  }
  
  def convert(t: Type): dzufferey.smtlib.Type = t match {
    case Bool => dzufferey.smtlib.Bool
    case Int => dzufferey.smtlib.Int
    case t @ (FSet(_) | FOption(_) | FMap(_, _) | Product(_)) =>
      Logger("dzufferey.smtlib", Debug, "loosing precision on type conversion: " + t)
      dzufferey.smtlib.UnInterpreted(Names.tpe(t))
    case Function(args, returns) => dzufferey.smtlib.Function(args.map(convert), convert(returns))
    case UnInterpreted(id) => dzufferey.smtlib.UnInterpreted(id)
    case Wildcard => dzufferey.smtlib.Wildcard
    case TypeVariable(v) => dzufferey.smtlib.TypeVariable(v)
  }
  
  def convert(t: dzufferey.smtlib.Type): Type = t match {
    case dzufferey.smtlib.Bool => Bool
    case dzufferey.smtlib.Int => Int
    case dzufferey.smtlib.Function(args, returns) => Function(args.map(convert), convert(returns))
    case dzufferey.smtlib.UnInterpreted(id) => UnInterpreted(id)
    case dzufferey.smtlib.Wildcard => Wildcard
    case dzufferey.smtlib.TypeVariable(v) => TypeVariable(v)
    case other => Logger.logAndThrow("dzufferey.smtlib", Error, "not supported: " + other)
  }

  def convert(s: Symbol): dzufferey.smtlib.Symbol = s match {
    case UnInterpretedFct(name, tpeOpt, tVars) =>
      dzufferey.smtlib.UnInterpretedFct(name, tpeOpt map convert, tVars map convert map (_.asInstanceOf[dzufferey.smtlib.TypeVariable]))
    case Not => dzufferey.smtlib.Not
    case And => dzufferey.smtlib.And
    case Or => dzufferey.smtlib.Or
    case Implies => dzufferey.smtlib.Implies
    case Eq => dzufferey.smtlib.Eq
    case Plus => dzufferey.smtlib.Plus
    case Minus => dzufferey.smtlib.Minus
    case Times => dzufferey.smtlib.Times
    case Divides => dzufferey.smtlib.Divides
    case Leq => dzufferey.smtlib.Leq
    case Geq => dzufferey.smtlib.Geq
    case Lt => dzufferey.smtlib.Lt
    case Gt => dzufferey.smtlib.Gt
    case other => Logger.logAndThrow("dzufferey.smtlib", Error, "not supported: " + other)
  }

  def convert(s: dzufferey.smtlib.Symbol): Symbol = s match {
    case dzufferey.smtlib.UnInterpretedFct(name, tpeOpt, tVars) =>
      UnInterpretedFct(name, tpeOpt map convert, tVars map convert map (_.asInstanceOf[TypeVariable]))
    case dzufferey.smtlib.Not => Not
    case dzufferey.smtlib.And => And
    case dzufferey.smtlib.Or => Or
    case dzufferey.smtlib.Implies => Implies
    case dzufferey.smtlib.Eq => Eq
    case dzufferey.smtlib.Plus => Plus
    case dzufferey.smtlib.Minus => Minus
    case dzufferey.smtlib.Times => Times
    case dzufferey.smtlib.Divides => Divides
    case dzufferey.smtlib.Leq => Leq
    case dzufferey.smtlib.Geq => Get
    case dzufferey.smtlib.Lt => Lt
    case dzufferey.smtlib.Gt => Gt
    case other => Logger.logAndThrow("dzufferey.smtlib", Error, "not supported: " + other)
  }

  def convert(f: Formula): dzufferey.smtlib.Formula = f match {
    case Literal(l: Boolean) => dzufferey.smtlib.Literal(l).setType(convert(f.tpe))
    case Literal(l: scala.Int) => dzufferey.smtlib.Literal(l).setType(convert(f.tpe))
    case Literal(l: Long) => dzufferey.smtlib.Literal(l).setType(convert(f.tpe))
    case Literal(l: Float) => dzufferey.smtlib.Literal(l).setType(convert(f.tpe))
    case Literal(l: Double) => dzufferey.smtlib.Literal(l).setType(convert(f.tpe))
    case Variable(v) => dzufferey.smtlib.Variable(v).setType(convert(f.tpe))
    case Application(fct, args) =>
      val args2 = args map convert
      dzufferey.smtlib.Application(convert(fct), args2).setType(convert(f.tpe))
    case Binding(bt, vs, f) =>
      val vs2 = vs map convert map (_.asInstanceOf[dzufferey.smtlib.Variable]) //this is bad but ...
      val f2 = convert(f)
      dzufferey.smtlib.Binding(convert(bt), vs2, f2).setType(convert(f.tpe))
    case other => Logger.logAndThrow("dzufferey.smtlib", Error, "not supported: " + other)
  }

  def convert(f: dzufferey.smtlib.Formula): Formula = f match {
    case dzufferey.smtlib.Literal(l) => Literal(l).setType(convert(f.tpe))
    case dzufferey.smtlib.Variable(v) => Variable(v).setType(convert(f.tpe))
    case dzufferey.smtlib.Application(fct, args) =>
      val args2 = args map convert
      Application(convert(fct), args2).setType(convert(f.tpe))
    case dzufferey.smtlib.Binding(bt, vs, f) =>
      val vs2 = vs map convert map (_.asInstanceOf[Variable]) //this is bad but ...
      val f2 = convert(f)
      Binding(convert(bt), vs2, f2)
  }

  protected val a = TypeVariable("A")
  protected val b = TypeVariable("B")
  
  protected def name(s: Symbol) = s match {
    case Fst => "fst"
    case Snd => "snd"
    case Trd => "trd"
    case Tuple => "tuple"
    case In => "in"
    case Contains => "contains"
    case Intersection => "intersection"
    case Union => "union"
    case SubsetEq => "subsetEq"
    case SupersetEq => "supersetEq"
    case i: InterpretedFct => i.symbol
    case other => Logger.logAndThrow("dzufferey.smtlib", Error, "unexpected: " + other)
  }

  def uninterpretSymbols(f: Formula): Formula = FormulaUtils.map( (f: Formula) => f match {
    case a @ Application(s @ (Union | Intersection | SubsetEq | SupersetEq |
                              In | Contains | Cardinality | FSome | FNone |
                              IsDefined | IsEmpty | Get | Tuple | Fst | Snd |
                              Trd | KeySet | LookUp | IsDefinedAt | Size), args) =>
      val params = FormulaUtils.typeParams(a)
      val nme = params.map(Names.tpe).mkString(name(s) + "_", "_", "")
      val s1 = UnInterpretedFct(nme, Some(Function( args.map(_.tpe), a.tpe)), Nil)
      Application(s1, args)
    case other => other
  }, f)


}
