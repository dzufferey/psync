package psync.logic

import psync.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

trait AxiomatizedTheory {
  def getAxioms(conjuncts: List[Formula]): List[Formula]
}

object AxiomatizedTheory extends AxiomatizedTheory {

  def theories = List(
    OptionAxioms,
    TupleAxioms,
    SetOperationsAxioms,
    MapUpdateAxioms
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
      acc ::= ForAll(tpl::args, Implies(Eq(tpl, app), Eq(Fst(tpl).setType(ts.head), args.head)))
    }
    if (size >= 2) {
      acc ::= ForAll(tpl::args, Implies(Eq(tpl, app), Eq(Snd(tpl).setType(ts(1)), args(1))))
    }
    if (size >= 3) {
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

  //We don't need to speak about cardinality here, just membership.
  //The cardinality constraints follow from the Venn region an membership.

  //∀ x,S,T. x ∈ S∪T ⇔ x ∈ S ∨ x ∈ T 
  def unionAxiom(tpe: Type) = {
    val x = Variable("x").setType(tpe)
    val s = Variable("S1").setType(FSet(tpe))
    val t = Variable("S2").setType(FSet(tpe))
    ForAll(List(x,s,t),
      Eq(In(x, Union(s,t)), Or(In(x, s), In(x, t))))
  }

  //∀ x,S,T. x ∈ S∩T ⇔ x ∈ S ∧ x ∈ T 
  def intersectionAxiom(tpe: Type) = {
    val x = Variable("x").setType(tpe)
    val s = Variable("S1").setType(FSet(tpe))
    val t = Variable("S2").setType(FSet(tpe))
    ForAll(List(x,s,t),
      Eq(In(x, Intersection(s,t)), And(In(x, s), In(x, t))))
  }

  //∀ x,S,T. x∈S ∧ S⊆T ⇒ x∈T
  def subsetAxiom(tpe: Type) = {
    val x = Variable("x").setType(tpe)
    val s = Variable("S1").setType(FSet(tpe))
    val t = Variable("S2").setType(FSet(tpe))
    ForAll(List(x,s,t),
      Implies(And(In(x,s), SubsetEq(s,t)), In(x,t)))
  }

  def getAxioms(conjuncts: List[Formula]): List[Formula] = {
    val f = And(conjuncts:_*)
    val setOps = FormulaUtils.collectSymbolsWithParams(f).collect{
        case p @ (Union | Intersection | SubsetEq | SupersetEq, _) => p
      }
    val setAxioms = setOps.toList.map{ case (sym, params) =>
      sym match {
        case Union => 
          assert(params.size == 1)
          SetOperationsAxioms.unionAxiom(params.head)
        case Intersection => 
          assert(params.size == 1)
          SetOperationsAxioms.intersectionAxiom(params.head)
        case SubsetEq =>
          assert(params.size == 1)
          SetOperationsAxioms.subsetAxiom(params.head)
        case _ =>
          Logger.logAndThrow("CL", Error, "missing set axioms for " + (sym,params))
      }
    }
    setAxioms
  }

}

object MapUpdateAxioms extends AxiomatizedTheory {


  def flatAxioms(kt: Type, vt: Type) = {
    val m1 = Variable("M1").setType(FMap(kt, vt))
    val m2 = Variable("M2").setType(FMap(kt, vt))
    val k = Variable("k").setType(kt)
    val k1 = Variable("k1").setType(kt)
    val k2 = Variable("k2").setType(kt)
    val k3 = Variable("k3").setType(kt)
    val v = Variable("v").setType(vt)
    val i = Variable("i").setType(Int)
    List(
      //∀ m,k,v. LookUp(Updated(m,k,v), k) = v
      ForAll(List(m1,m2,k1,k2,v),
        Implies(
          And(
            Eq(k1, k2),
            Eq(Updated(m1, k1, v), m2)
          ),
          Eq(LookUp(m2, k2), v))),
      //∀ m,k1,v,k2. k1≠k2 ⇒ LookUp(Updated(m,k1,v), k2) = LookUp(m,k2)
      ForAll(List(m1,m2,k1,k2,k3,v),
        Implies(
          And(
            Not(Eq(k1, k2)),
            Eq(k2, k3),
            Eq(Updated(m1, k1, v), m2),
            In(k2, KeySet(m1))
          ),
          Eq(LookUp(m2, k2), LookUp(m1,k3)))),
      //∀ m,k,v. KeySet(Updated(m,k,v)) = KeySet(m) ∪ {k}
      ForAll(List(m1,m2,k1,k2,v),
        Implies(
          Eq(m2, Updated(m1, k1, v)),
          Eq(In(k2, KeySet(m2)), Or(In(k2, KeySet(m1)), Eq(k1, k2)))
        )),
      //the next axiom has a funny shape to be local (hopefully)
      ForAll(List(m1,m2,k1,k2,v,i),
        Implies(
          And(
            Eq(m2, Updated(m1, k1, v)),
            Lt(Cardinality(KeySet(m1)), i)
          ),
          Leq(Cardinality(KeySet(m2)), i)
        ))
    )
  }

  def getAxioms(conjuncts: List[Formula]): List[Formula] = {
    val f = And(conjuncts:_*)
    val updates = FormulaUtils.collectSymbolsWithParams(f).collect{
        case p @ (Updated, _) => p
      }
    val updAxioms = updates.toList.flatMap{
      case (Updated, List(kt, vt)) =>
        flatAxioms(kt, vt)
      case err =>
        Logger.logAndThrow("CL", Error, "map update: " + err)
    }
    updAxioms
  }
}
