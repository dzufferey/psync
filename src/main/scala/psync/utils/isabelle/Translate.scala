package psync.utils.isabelle

import psync.formula._
import info.hupel.isabelle._
import info.hupel.isabelle.pure.{Type => IType, _}
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._


// inspired by
// https://github.com/epfl-lara/leon/blob/master/src/main/scala/leon/solvers/isabelle/Translator.scala

// "Set.UNIV"
// "Finite_Set.finite"

object TranslateFormula {

  def cleanName(id: String): String = {
    dzufferey.smtlib.Printer.printable(id)
  }

  def mkConst(what: String) = Const(what, Typ.dummyT)

  def universe(t: Type) = {
    val tt = TranslateType.to(FSet(t))
    Const("Set.UNIV", tt)
  }
  
  def finite(t: Type) = {
    val f = mkConst("Finite_Set.finite")
    val u = universe(t)
    App(f, u)
  }

  // Why do I get a type error with that ?
  def pureImplies(args: Term*) = {
    val i = mkConst("Pure.imp")
    args.reduceRight( (t, acc) => App(App(i, t), acc) )
  }

  def interpreted(s: InterpretedFct) = mkConst(s match {
    case Not =>         "HOL.Not"
    case And =>         "HOL.conj"
    case Or =>          "HOL.disj"
    case Implies =>     "HOL.implies" // XXX an alternative is "Pure.imp"
    case Eq =>          "HOL.eq"
    case Neq =>         "HOL.not_equal"
    case Cardinality => "Finite_Set.card"
    case Lt =>          "Orderings.ord_class.less"
    case Leq =>         "Orderings.ord_class.less_eq"
    case Gt =>          "Orderings.ord_class.greater"
    case Geq =>         "Orderings.ord_class.greater_eq"
    case Plus =>        "Groups.plus_class.plus"
    case Minus =>       "Groups.minus_class.minus"
    case Times =>       "Groups.times_class.times"
    case Divides =>     "Rings.divide_class.divide"
    case Intersection =>"Set.inter"
    case Union =>       "Set.union"
    case In =>          "Set.member"
    case SubsetEq =>    "Set.subset_eq"
    case SupersetEq =>  "Set.supset_eq"
    case Tuple =>       "Product_Type.Pair"
    case Fst =>         "Product_Type.prod.fst"
    case Snd =>         "Product_Type.prod.snd"
    case FSome =>       "Option.option.Some"
    case FNone =>       "Option.option.None"
    case Get =>         "Option.option.the"
    case IsEmpty =>     "Option.is_none"
    case KeySet =>      "Finite_Map.fmdom"
    case Updated =>     "Finite_Map.fmupd"
    case other =>
        Logger.logAndThrow("isabelle.TranslateFormula.interpreted",
                           Error,
                           "no corresponding symbol for " + other)
  })

  def associative(i: InterpretedFct) = i match {
    case And | Or | Plus | Times | Union | Intersection => true
    case _ => false
  }

  val zero: Term = Const("Groups.zero_class.zero", Typ.dummyT)

  // XXX is there a better way ?
  def mkNat(l: Long): Term = {
    Logger.assert(l >= 0, "isabelle.TranslateFormula.makeNat", "not a ℕ: " + l)
    val s = Const("Nat.Suc", Typ.dummyT)
    def suc(t: Term) = App(s, t)
    var i = 0l
    var acc = zero
    while (i < l) {
      i += 1
      acc = suc(acc)
    }
    acc
  }
  
  def mkAbs(binder: Term, vs: List[Variable], body: Formula, bound: List[Variable]): Term = {
    val bound2 = vs.reverse ::: bound // XXX is that the right order
    val bodyTerm = to(body, bound2)
    vs.foldRight(bodyTerm){ (v, acc) =>
      val t = TranslateType.to(v.tpe)
      val id0 = cleanName(v.name)
      App(binder, Abs(id0, t, acc))
    }
  }

  def mkApp(i: InterpretedFct, args: Term*) = {
    val s = interpreted(i)
    if (associative(i)) {
      args.reduceRight( (t, acc) => App(App(s, t), acc) )
    } else {
      args.foldLeft(s: Term)( (acc, t) => App(acc, t) )
    }
  }

  def to(f: Formula, bound: List[Variable]): Term = f match {
    case UnitLit() =>
      mkConst("Product_Type.Unity")
    case True() =>
      mkConst("HOL.True")
    case False() =>
      mkConst("HOL.False")
    case IntLit(i) =>
      val n = mkNat(i.abs)
      if (i >= 0) n else App(mkConst("Groups.uminus_class.uminus"), n)
    case v @ Variable(_) =>
      bound.indexOf(v) match {
        case -1 =>
          val id0 = cleanName(v.name)
          val t = TranslateType.to(f.tpe)
          Free(id0, t)
        case n =>
          Bound(n)
      }
    case Minus(f1) =>
      val t1 = to(f1, bound)
      App(mkConst("Groups.uminus_class.uminus"), t1)
    case Contains(f1,f2) =>
      to(In(f2,f1), bound)
    case IsDefined(f1) =>
      to(Not(IsEmpty(f1)), bound)
    case Tuple(fs @ _*) =>
      Logger.assert(fs.length == 2 || fs.length == 3, "isabelle.TranslateFormula.to", "only Pairs and Triple supported: " + f)
      val ts = fs.map(to(_, bound))
      val tpl = interpreted(Tuple)
      ts.reduceRight( (t, acc) => App(App(tpl, t), acc) ) 
    case Snd(f1) =>
      val t1 = to(f1, bound)
      val s = interpreted(Snd)
      f1.tpe match {
        case Product(lst) =>
          lst.length match {
            case 2 =>
              App(s, t1)
            case 3 =>
              App(interpreted(Fst), App(s, t1))
            case n =>
              Logger.logAndThrow("isabelle.TranslateFormula.to", Error, "expected pair/triple, found " + n + "-tuple")
          }
        case other =>
          Logger.logAndThrow("isabelle.TranslateFormula.to", Error, "expected Product, found: " + other)
      }
    case Trd(f1) =>
      val t1 = to(f1, bound)
      val s = interpreted(Snd)
      App(s, App(s, t1))
    case LookUp(f1, f2) =>
      val t1 = to(f1, bound)
      val t2 = to(f2, bound)
      val l = interpreted(LookUp)
      val get = interpreted(Get)
      App(get, App(App(l, t1), t2))
    case IsDefinedAt(f1, f2) =>
      to(In(f2,KeySet(f1)), bound)
    case Size(f1) =>
      to(Cardinality(KeySet(f1)), bound)
    case Updated(map, key, value) =>
      val m = to(map, bound)
      val k = to(key, bound)
      val v = to(value, bound)
      val u = interpreted(Updated)
      App(App(App(u, v), k), m)
    case FNone() =>
      interpreted(FNone)
    case Application(i: InterpretedFct, fs) =>
      Logger.assert(fs.nonEmpty, "isabelle.TranslateFormula.to", "no arg for: " + f)
      val ts = fs.map(to(_, bound))
      mkApp(i, ts:_*)
    case Application(UnInterpretedFct(symbol, tpe, tparams), fs) =>
      val ts = fs.map(to(_, bound))
      val symT = tpe.map(TranslateType.to).getOrElse(Typ.dummyT)
      val id0 = cleanName(symbol)
      val cst = Free(id0, symT)
      // val cst = Const(id0, symT) // XXX we need to declare const before using them
      ts.foldLeft(cst:Term)( (acc, t) => App(acc, t) )
    case Binding(bt, vs, f1) =>
      val cst = bt match {
        case ForAll => mkConst("HOL.All")
        case Exists => mkConst("HOL.Ex")
        case Comprehension => mkConst("Set.Collect")
      }
      mkAbs(cst, vs, f1, bound)
    case _ =>
      Logger.logAndThrow("isabelle.TranslateFormula.to", Error, "unexptected: " + f)
  }
  
  def apply(f: Formula) = to(f, Nil)

  def from(f: Term): Formula = f match {
    case _ =>
      ???
      //Logger.logAndThrow("isabelle.TranslateFormula.from", Error, "unexptected: " + f)
  }

}


// inspired by
// https://github.com/epfl-lara/leon/blob/master/src/main/scala/leon/solvers/isabelle/Types.scala

//Warning: for the moment, ℤ are interpreted as ℕ
object TranslateType {

  def to(t: Type): Typ = t match {
    case UnitT() => IType("Product_Type.unit", Nil)
    case Bool => IType("HOL.bool", Nil)
    //case Int => IType("Int.int", Nil)
    case Int => IType("Nat.nat", Nil) // XXX
    case FSet(f1) =>
      val t1 = to(f1)
      IType("Set.set", List(t1))
    case FMap(f1,f2) =>
      val t1 = to(f1)
      val t2 = to(f2)
      IType("fun", List(t1, IType("Option.option", List(t2))))
    case FOption(f1) =>
      val t1 = to(f1)
      IType("Option.option", List(t1))
    case Product(lst) =>
      Logger.assert(lst.length >= 2, "isabelle.TranslateType.to", "Product is too short: " + t)
      val ts = lst.map(to)
      ts.reduceRight( (t, acc) => IType("Product_Type.prod", List(t, acc)) )
    case Function(args, ret) =>
      val tas = args.map(to)
      val tr = to(ret)
      tas.foldRight(tr)( (t, acc) => IType("fun", List(t, acc)) )
    case UnInterpreted(id) =>
      val id0 = TranslateFormula.cleanName(id)
      TFree("'"+id0, List("HOL.type"))
    case TypeVariable(id) =>
      val id0 = TranslateFormula.cleanName(id)
      TFree(id0, List("HOL.type"))
    case Wildcard => Typ.dummyT
  }

  def from(t: Typ): Type = t match {
    case _ =>
      ???
  }

}
