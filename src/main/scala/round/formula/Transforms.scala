package round.formula

import round.utils.Namer

object Copier {

  def Literal[T <: AnyVal](from: Formula, value: T) = from match {
    case f @ Literal(v0) if v0 == value => f
    case _ =>
      val l2 = round.formula.Literal(value)
      l2.tpe = from.tpe
      l2
  }

  def Variable(from: Formula, name: String) = from match {
    case v @ Variable(name0) if name0 == name => v
    case _ =>
      val v2 = round.formula.Variable(name)
      v2.tpe = from.tpe
      v2
  }

  def Application(from: Formula, fct: Symbol, args: List[Formula]) = from match {
    case a @ Application(fct0, args0) if fct == fct0 && args == args0 => a
    case _ =>
      val a2 = round.formula.Application(fct, args)
      a2.tpe = from.tpe
      a2
  }

  def Binding(from: Formula, bt: BindingType, vs: List[Variable], f: Formula) = from match {
    case b @ Binding(bt0, vs0, f0) if bt0 == bt && vs0 == vs && f0 == f => b
    case _ =>
      val b2 = round.formula.Binding(bt, vs, f)
      b2.tpe = from.tpe
      b2
  }

}

abstract class Traverser {

  def traverse(f: Formula): Unit = f match {
    case Literal(value) => ()
    case Variable(v) => ()
    case Application(fct, args) =>
      //traverse(fct)
      args foreach traverse
    case Binding(_, vs, f) =>
      vs foreach traverse
      traverse(f)
  }

}

abstract class Transformer {

  def transform(f: Formula): Formula = f match {
    case l @ Literal(_) => l
    case v @ Variable(_) => v
    case f @ Application(fct, args) =>
      val args2 = args map transform
      Copier.Application(f, fct, args2)
    case b @ Binding(bt, vs, f) =>
      val vs2 = vs map transform map (_.asInstanceOf[Variable]) //this is bad but ...
      val f2 = transform(f)
      Copier.Binding(b, bt, vs2, f2)
  }

}

class Mapper(fct: Formula => Formula) extends Transformer {
  override def transform(f: Formula): Formula = f match {
    case b @ Binding(bt, vs, f) =>
      def fct2(f: Formula): Formula = {
        if (vs.exists(v => f == v)) f else fct(f)
      }
      val m2 = new Mapper(fct2)
      fct(Copier.Binding(b, bt, vs, m2.transform(f)))
    case other =>
      fct(super.transform(f))
  }
}

class MapperAll(fct: Formula => Formula) extends Transformer {
  override def transform(f: Formula): Formula = fct(super.transform(f))
}

class MapperSym(fct: Symbol => Symbol ) extends Transformer {
  override def transform(f: Formula): Formula = super.transform(f) match {
    case a @ Application(sym, args) => Copier.Application(a, fct(sym), args)
    case other => other
  }
}

class Alpha(map: Map[Variable, Variable]) extends Transformer {

  lazy val from = map.keySet
  lazy val to = Set[Variable](map.values.toSeq :_*)

  override def transform(f: Formula): Formula = f match {
    case bind @ Binding(b, vars, f) =>
      val varSet = Set[Variable](vars:_*)
      val captured = varSet intersect to //those are to be renamed
      val overriding = varSet intersect from
      val avoidCapture = captured.toList map (v => (v, Variable(Namer(v.name))))
      val newMap = map -- overriding ++ avoidCapture
      val args2 = vars map (v => newMap.getOrElse(v,v))
      val inner = (new Alpha(newMap)).transform(f)
      Copier.Binding(bind, b, args2, inner)
    case v @ Variable(_) => map.getOrElse(v,v)
    case _ => super.transform(f)
  }

}

class AlphaAll(map: Map[Variable, Variable]) extends Transformer {

  override def transform(f: Formula): Formula = f match {
    case v @ Variable(_) => map.getOrElse(v,v)
    case _ => super.transform(f)
  }

}

//pull UnInterpretedFct out of application: y = f(g(x) → y = f(z) ∧ z = g(y)
class Purifier extends Transformer {

  var collected: List[Formula] = Nil
  var csts: List[Variable] = Nil

  def dummy(name:String, tpe: Type) = Variable(Namer("__"+name)).setType(tpe)

  override def transform(f: Formula): Formula = super.transform(f) match {
    case b @ Binding(bt @ (ForAll | Comprehension), vs, f) if csts != Nil =>
      assert(collected == Nil)
      val f2 = Copier.Binding(b, bt, vs, Exists(csts, f))
      csts = Nil
      f2
    case ap @ Application(UnInterpretedFct(name,_,_), _) =>
      val v = dummy(name, ap.tpe)
      collected = Eq(v, ap) :: collected
      csts = v :: csts
      v
    case bool if !collected.isEmpty && bool.tpe == Bool =>
      val purified = collected.foldLeft(bool)(And(_,_))
      collected = Nil
      purified
    case other => other
  }

}

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
    case Application(And, lst) =>Copier. Application(f, And, lst.flatMap(flatten1(And, _)))
    case Application(Or, lst) =>Copier. Application(f, Or, lst.flatMap(flatten1(Or, _)))
    case Application(other, lst) =>Copier. Application(f, other, lst map flatten)
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

}

