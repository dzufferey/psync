package round.formula


object Simplify {

  //removes functions that can be expressed in terms of other
  def normalize(f: Formula) = normalizer.transform(f)
  private val normalizer = new Mapper(normalizef)
  private def normalizef(f: Formula): Formula = f match {
    case Implies(List(a,b)) => Copier.Application(f, Or, List(Copier.Application(a, Not, List(a)),b))
    case Neq(List(a,b)) => Copier.Application(f, Not, List(Copier.Application(f, Eq, List(a,b))))
    case Geq(List(a,b)) => Copier.Application(f, Not, List(Copier.Application(f, Lt, List(b,a))))
    case Leq(List(a,b)) => Copier.Application(f, Not, List(Copier.Application(f, Lt, List(b,a))))
    case Gt(List(a,b)) =>  Copier.Application(f, Lt, List(b,a))
    case SupersetEq(List(a,b)) => Copier.Application(f, SubsetEq, List(b,a))
    case Contains(List(a,b)) => Copier.Application(f, In, List(b,a))
    case other => other
  }


  //negation normal form (assumes normaled formula)
  def nnf(f: Formula, neg: Boolean = false): Formula = f match {
    case Binding(ForAll, vs, f2) =>
      val bt = if (neg) Exists else ForAll
      Copier.Binding(f, bt, vs, nnf(f2, neg))
    case Binding(Exists, vs, f2) =>
      val bt = if (neg) ForAll else Exists
      Copier.Binding(f, bt, vs, nnf(f2, neg))
    case Application(Not, List(f2)) =>
      nnf(f2, !neg)
    case Application(And, args) => 
      val fct = if (neg) Or else And
      val args2 = args.map(nnf(_, neg))
      Copier.Application(f, fct, args2)
    case Application(Or, args) => 
      val fct = if (neg) And else Or
      val args2 = args.map(nnf(_, neg))
      Copier.Application(f, fct, args2)
    case other =>
      assert(other.tpe == Bool)
      if (neg) Not(other) else other
  }
  

  //prenex normal form
  def pnf(f: Formula) = {
    val f2 = boundVarUnique(nnf(f))
    //TODO
    sys.error("TODO")
  }

  //makes all the bound variables different
  def boundVarUnique(f: Formula): Formula = {
    sys.error("TODO")
  }

  
  def simplifySet(f: Formula): Formula = {
    sys.error("TODO")
  }
  
  def simplifyInt(f: Formula): Formula = {
    //TODO division: from 'x > 2n/3' to '3x > 2n'
    //TODO constant folding
    sys.error("TODO")
  }
  
  def simplifyBool(f: Formula): Formula = {
    sys.error("TODO")
  }

  def simplify(f: Formula): Formula = {
    sys.error("TODO")
  }


}
