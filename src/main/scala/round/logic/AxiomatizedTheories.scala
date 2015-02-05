package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object OptionAxioms {

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
    axms//.map(Typer(_).get)
  }

  def addAxioms(conjuncts: List[Formula]): List[Formula] = {
    //collect the option types, get the parameters, and mkAxioms
    val tpes = conjuncts.foldLeft(Set[Type]())( _ ++ FormulaUtils.collectTypes(_) )
    val opts = tpes.toList.collect{ case FOption(t) => t }
    val axms = opts.flatMap(mkAxioms)
    conjuncts ::: axms
  }

}

object TupleAxioms {

  private def mkAxioms(ts: List[Type]): List[Formula] = {
    val args = ts.zipWithIndex.map{ case (t,i) => Variable("tplArg"+i).setType(t) }
    val tpl = Variable("tpl").setType(Product(ts))
    val app = Application(Tuple, args).setType(Product(ts))
    var acc: List[Formula] = Nil
    if (ts.size >= 1) {
      acc = ForAll(tpl::args, Implies(Eq(tpl, app), Eq(Fst(tpl).setType(ts(0)), args(0)))) :: acc
    }
    if (ts.size >= 2) {
      acc = ForAll(tpl::args, Implies(Eq(tpl, app), Eq(Snd(tpl).setType(ts(1)), args(1)))) :: acc
    }
    if (ts.size >= 3) {
      acc = ForAll(tpl::args, Implies(Eq(tpl, app), Eq(Trd(tpl).setType(ts(2)), args(2)))) :: acc
    }
    acc//.map(Typer(_).get)
  }

  def addAxioms(conjuncts: List[Formula]): List[Formula] = {
    //collect the option types, get the parameters, and mkAxioms
    val tpes = conjuncts.foldLeft(Set[Type]())( _ ++ FormulaUtils.collectTypes(_) )
    val opts = tpes.toList.collect{ case Product(ts) => ts }
    val axms = opts.flatMap(mkAxioms)
    conjuncts ::: axms
  }

}
