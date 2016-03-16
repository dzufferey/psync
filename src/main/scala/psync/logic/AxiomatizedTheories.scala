package psync.logic

import psync.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

trait AxiomatizedTheory {
  def getAxioms(conjuncts: List[Formula]): List[Formula]
}

object AxiomatizedTheory extends AxiomatizedTheory {
  val theories = List(
    OptionAxioms,
    TupleAxioms,
    SetOperationsAxioms
  )

  def getAxioms(conjuncts: List[Formula]): List[Formula] = {
    theories.flatMap( _.getAxioms(conjuncts) )
  }

}

object OptionAxioms extends AxiomatizedTheory {

  private def mkAxioms(t: Type): List[Formula] = {
    //println("mkAxioms with " + t)
    val x = Variable("x").setType(t)
    val y = Variable("y").setType(FOption(t))
    val none = FNone().setType(FOption(t))
    val axms = List(
      //get is the inverse of some
      ForAll(List(x,y), Implies(Eq(FSome(x).setType(FOption(t)),y), Eq(Get(y).setType(t), x))),
      //None is different from some
      ForAll(List(x), Not(Eq(FSome(x).setType(FOption(t)),none))),
      //IsDefined
      ForAll(List(x,y), Implies(Eq(FSome(x).setType(FOption(t)),y), IsDefined(y))),
      //IsEmpty
      Not(IsDefined(none))  //IsEmpty(none)
      //def xor defined, implied as IsEmpty is replaced by ¬IsDefined
      //ForAll(List(y), Or(And(IsDefined(y), Not(IsEmpty(y))), And(Not(IsDefined(y)), IsEmpty(y))))
    )
    axms
  }

  def getAxioms(conjuncts: List[Formula]): List[Formula] = {
    //collect the option types, get the parameters, and mkAxioms
    val tpes = conjuncts.foldLeft(Set[Type]())( _ ++ FormulaUtils.collectTypes(_) )
    val opts = tpes.toList.collect{ case FOption(t) => t }
    val axms = opts.flatMap(mkAxioms)
    axms
  }

}

object TupleAxioms extends AxiomatizedTheory {

  private def mkAxioms(ts: List[Type]): List[Formula] = {
    val args = ts.zipWithIndex.map{ case (t,i) => Variable("tplArg"+i).setType(t) }
    val tpl = Variable("tpl").setType(Product(ts))
    val app = Application(Tuple, args).setType(Product(ts))
    var acc: List[Formula] = Nil
    val size = ts.size
    if (size >= 1) {
      //acc ::= ForAll(args, Eq(Fst(app).setType(ts(0)), args(0)))
      acc ::= ForAll(tpl::args, Implies(Eq(tpl, app), Eq(Fst(tpl).setType(ts(0)), args(0))))
    }
    if (size >= 2) {
      //acc ::= ForAll(args, Eq(Snd(app).setType(ts(1)), args(1)))
      acc ::= ForAll(tpl::args, Implies(Eq(tpl, app), Eq(Snd(tpl).setType(ts(1)), args(1))))
    }
    if (size >= 3) {
      //acc ::= ForAll(args, Eq(Trd(app).setType(ts(2)), args(2)))
      acc ::= ForAll(tpl::args, Implies(Eq(tpl, app), Eq(Trd(tpl).setType(ts(2)), args(2))))
    }
    acc
  }

  def getAxioms(conjuncts: List[Formula]): List[Formula] = {
    //collect the option types, get the parameters, and mkAxioms
    val tpes = conjuncts.foldLeft(Set[Type]())( _ ++ FormulaUtils.collectTypes(_) )
    val opts = tpes.toList.collect{ case Product(ts) => ts }
    val axms = opts.flatMap(mkAxioms)
    axms
  }
}

object SetOperationsAxioms extends AxiomatizedTheory {

  //TODO should we have reflexivity, symmetry, etc., ?

  //∀ x,S,T. x ∈ S∪T ⇔ x ∈ S ∨ x ∈ T 
  def unionAxiom(tpe: Type) = {
    val x = Variable("x").setType(tpe)
    val s = Variable("S1").setType(FSet(tpe))
    val t = Variable("S2").setType(FSet(tpe))
    ForAll(List(x,s,t),
      Eq(In(x, Union(s,t)), Or(In(x, s), In(x, t))))
  }

  //∀ S,T,U. U = S∪T ⇒ |S∪T| ≥ |S| ∧ |S∪T| ≥ |T|
  def unionCardAxiom(tpe: Type) = {
    val s = Variable("S1").setType(FSet(tpe))
    val t = Variable("S2").setType(FSet(tpe))
    ForAll(List(s,t),
      And(Geq(Cardinality(Union(s,t)),Cardinality(s)),
          Geq(Cardinality(Union(s,t)),Cardinality(t))))
  }

  //∀ x,S,T. x ∈ S∩T ⇔ x ∈ S ∧ x ∈ T 
  def intersectionAxiom(tpe: Type) = {
    val x = Variable("x").setType(tpe)
    val s = Variable("S1").setType(FSet(tpe))
    val t = Variable("S2").setType(FSet(tpe))
    ForAll(List(x,s,t),
      Eq(In(x, Intersection(s,t)), And(In(x, s), In(x, t))))
  }

  //∀ S,T,U. U = S∩T ⇒ |U| ≤ |S| ∧ |U| ≤ |T| 
  def intersectionCardAxiom(tpe: Type) = {
    val s = Variable("S1").setType(FSet(tpe))
    val t = Variable("S2").setType(FSet(tpe))
    ForAll(List(s,t),
      And(Leq(Cardinality(Intersection(s,t)),Cardinality(s)),
          Leq(Cardinality(Intersection(s,t)),Cardinality(t))))
  }

  //∀ x,S,T. x∈S ∧ S⊆T ⇒ x∈T
  def subsetAxiom(tpe: Type) = {
    val x = Variable("x").setType(tpe)
    val s = Variable("S1").setType(FSet(tpe))
    val t = Variable("S2").setType(FSet(tpe))
    ForAll(List(x,s,t),
      Implies(And(In(x,s), SubsetEq(s,t)), In(x,t)))
  }

  //∀ S,T. S⊆T ⇒ |S| ≤ |T|
  def subsetCardAxiom(tpe: Type) = {
    val s = Variable("S1").setType(FSet(tpe))
    val t = Variable("S2").setType(FSet(tpe))
    ForAll(List(s,t),
      Implies(SubsetEq(s, t),
              Leq(Cardinality(s),Cardinality(t))))
  }
  
  def getAxioms(conjuncts: List[Formula]): List[Formula] = {
    val f = And(conjuncts:_*)
    val setOps = FormulaUtils.collectSymbolsWithParams(f).collect{
        case p @ (Union | Intersection | SubsetEq | SupersetEq, _) => p
      }
    val setAxioms = setOps.toList.flatMap{ case (sym, params) =>
      sym match {
        case Union => 
          assert(params.size == 1)
          List(
            SetOperationsAxioms.unionCardAxiom(params.head),
            SetOperationsAxioms.unionAxiom(params.head)
          )
        case Intersection => 
          assert(params.size == 1)
          List(
            SetOperationsAxioms.intersectionCardAxiom(params.head),
            SetOperationsAxioms.intersectionAxiom(params.head)
          )
        case SubsetEq =>
          assert(params.size == 1)
          List(
            SetOperationsAxioms.subsetCardAxiom(params.head),
            SetOperationsAxioms.subsetAxiom(params.head)
          )
        case _ =>
          Logger("CL", Warning, "TODO addSetAxioms for " + (sym,params))
          Nil
      }
    }
    setAxioms
  }


}
