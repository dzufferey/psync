package round.formula

import dzufferey.utils.Namer

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
    case IsEmpty(List(a)) => Copier.Application(f, Not, List(Copier.Application(f, IsDefined, List(a))))
    case other => other
  }


  //negation normal form (assumes normalized formula)
  //TODO also recurse in Comprehension
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
  //TODO also recurse in Comprehension
  def pnf(f: Formula) = {
    val f2 = boundVarUnique(nnf(f))
    def merge(lst: List[List[(BindingType, List[Variable])]], exists: Boolean): List[(BindingType, List[Variable])] = {
      val (vs, rest) = lst.foldLeft( (List[Variable](), List[List[(BindingType, List[Variable])]]()) )( (acc, f) => {
        f match {
          case (ForAll, vs) :: xs => if (!exists) (vs ::: acc._1, xs :: acc._2) else (acc._1, f :: acc._2)
          case (Exists, vs) :: xs => if ( exists) (vs ::: acc._1, xs :: acc._2) else (acc._1, f :: acc._2)
          case Nil => acc
          case _ => sys.error("should not happen")
        }
      })
      val suffix = if (!rest.isEmpty) merge(rest, !exists) else Nil
      if (vs.isEmpty) suffix
      else if (exists) (Exists, vs) :: suffix
      else (ForAll, vs) :: suffix
    }
    def pull(f: Formula, exists: Boolean): (List[(BindingType, List[Variable])], Formula) = f match {
      case Binding(ForAll, vs, f2) =>
        val (b1, f3) = pull(f2, false)
        val b2 = b1 match {
          case (ForAll, vs2) :: bs => (ForAll, vs ::: vs2) :: bs
          case _ => (ForAll, vs) :: b1 
        }
        (b2, f3)

      case Binding(Exists, vs, f2) =>
        val (b1, f3) = pull(f2, true)
        val b2 = b1 match {
          case (Exists, vs2) :: bs => (Exists, vs ::: vs2) :: bs
          case _ => (Exists, vs) :: b1 
        }
        (b2, f3)

      case Application(And, args) => 
        val (binders, args2) = args.map(pull(_, exists)).unzip
        val merged = merge(binders, exists)
        val f2 = Copier.Application(f, And, args2)
        (merged, f2)

      case Application(Or, args) => 
        val (binders, args2) = args.map(pull(_, exists)).unzip
        val merged = merge(binders, exists)
        val f2 = Copier.Application(f, Or, args2)
        (merged, f2)

      case other =>
        (Nil, other)
    }
    val (quantifiers, qf) = pull(f2, true)
    quantifiers.foldRight(qf)( ( p, acc) => Binding(p._1, p._2, acc).setType(Bool) )
  }

  def isPnf(f: Formula): Boolean = {
    def rmQ(f: Formula): Formula = f match {
      case Binding(ForAll | Exists, _, f) => rmQ(f)
      case f => f
    }
    def hasQ(f: Formula): Boolean = f match {
      case Binding(ForAll | Exists, _, _) => true
      case Application(_, args) => args.exists(hasQ)
      case _ => false
    }
    !hasQ(rmQ(f))
  }

  //makes all the bound variables different
  def boundVarUnique(f: Formula): Formula = {
    def s(subst: Map[String, String], v: Variable): Variable = {
      if (subst contains v.name) Copier.Variable(v, subst(v.name))
      else v
    }
    def process(f: Formula, used: Set[String], subst: Map[String, String]): (Formula, Set[String]) = f match {
      case Binding(bt, vs, f2) =>
        val masking = vs.filter(v => used(v.name))
        val subst2 = subst ++ masking.map( v => (v.name, Namer(v.name)) )
        val vs2 = vs.map(s(subst2,_))
        val used2 = vs2.foldLeft(used)(_ + _.name)
        val (f3, used3) = process(f2, used2, subst2)
        (Copier.Binding(f, bt, vs2, f3), used3)
      case Application(fct, args) => 
        val (args2, used2) = args.foldRight( (List[Formula](), used) )( (arg, acc) => {
          val (args, used) = acc
          val (arg2, used2) = process(arg, used, subst)
          (arg2 :: args, used2)
        }) 
        (Copier.Application(f, fct, args2), used2)
      case v @ Variable(_) => (s(subst, v), used)
      case other => (other, used)
    }
    process(f,
            Set.empty[String],
            Map.empty[String, String])._1
  }

  //TODO cnf/dnf

  
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
