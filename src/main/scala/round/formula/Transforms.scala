package round.formula

import round.utils.Namer
import scala.collection.immutable.{Set => SSet}

object Copier {

  def Literal[T](from: Formula, value: T) = {
    val l2 = round.formula.Literal(value)
    l2.tpe = from.tpe
    l2
  }

  def Variable(from: Formula, name: String) = {
    val v2 = round.formula.Variable(name)
    v2.tpe = from.tpe
    v2
  }

  def Application(from: Formula, fct: Symbol, args: List[Formula]) = {
    val a2 = round.formula.Application(fct, args)
    a2.tpe = from.tpe
    a2
  }

  def Binding(from: Formula, bt: BindingType, vs: List[Variable], f: Formula) = {
    val b2 = round.formula.Binding(bt, vs, f)
    b2.tpe = from.tpe
    b2
  }

  def Exists(from: Formula, vs: List[Variable], f: Formula) = {
    val e2 = round.formula.Exists(vs, f)
    e2.tpe = from.tpe
    e2
  }

  def ForAll(from: Formula, vs: List[Variable], f: Formula) = {
    val a2 = round.formula.ForAll(vs, f)
    a2.tpe = from.tpe
    a2
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
      //val fct2 = transform(fct)
      val args2 = args map transform
      Copier.Application(f, fct/*2*/, args2)
    case b @ Binding(bt, vs, f) =>
      val vs2 = vs map transform map (_.asInstanceOf[Variable]) //this is fairly bad but ...
      val f2 = transform(f)
      Copier.Binding(b, bt, vs2, f2)
  }

}

class Mapper(fct: Formula => Formula) extends Transformer {
  override def transform(f: Formula): Formula = fct(super.transform(f))
}

class Alpha(map: Map[Variable, Variable]) extends Transformer {

  lazy val from = map.keySet
  lazy val to = SSet[Variable](map.values.toSeq :_*)

  override def transform(f: Formula): Formula = f match {
    case bind @ Binding(b, vars, f) =>
      val varSet = SSet[Variable](vars:_*)
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

object FormulaUtils {

  def alpha(map: Map[Variable, Variable], f: Formula): Formula = {
    val a = new Alpha(map)
    a.transform(f)
  }

  def alphaAll(map: Map[Variable, Variable], f: Formula): Formula = {
    val a = new AlphaAll(map)
    a.transform(f)
  }

  /** Requires that bound variables are bound to variables (otherwise fails) */
  def map(fct: Formula => Formula, f: Formula): Formula = {
    val m = new Mapper(fct)
    m.transform(f)
  }

  /** Rename all free variables that appears in the formula. */
  def prime(f: Formula): (Formula, Map[Variable, Variable]) = {
    val free = f.freeVariables
    val mapping = (Map[Variable, Variable]() /: free)( (acc, v) => acc + (v -> Variable(Namer(v.name))))
    (alpha(mapping, f), mapping)
  }

}

