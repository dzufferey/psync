package psync.formula

import dzufferey.utils.Namer

object Copier {

  def Literal[T <: AnyVal](from: Formula, value: T) = from match {
    case f @ Literal(v0) if v0 == value => f
    case _ => psync.formula.Literal(value).setType(from.tpe)
  }

  def Variable(from: Formula, name: String) = from match {
    case v @ Variable(name0) if name0 == name => v
    case _ => psync.formula.Variable(name).setType(from.tpe)
  }

  def Application(from: Formula, fct: Symbol, args: List[Formula]) = from match {
    case a @ Application(fct0, args0) if fct == fct0 && args == args0 => a
    case _ => psync.formula.Application(fct, args).setType(from.tpe)
  }

  def Binding(from: Formula, bt: BindingType, vs: List[Variable], f: Formula) = from match {
    case b @ Binding(bt0, vs0, f0) if bt0 == bt && vs0 == vs && f0 == f => b
    case _ => psync.formula.Binding(bt, vs, f).setType(from.tpe)
  }

}

abstract class Traverser {

  def traverse(f: Formula): Unit = f match {
    case Literal(value) => ()
    case Variable(v) => ()
    case Application(fct, args) =>
      args foreach traverse
    case Binding(_, vs, f) =>
      vs foreach traverse
      traverse(f)
  }

}

abstract class TraverserWithScope {

  def traverse(bound: Set[Variable], f: Formula): Unit = f match {
    case Literal(value) => ()
    case Variable(v) => ()
    case Application(fct, args) =>
      args.foreach(traverse(bound,_))
    case Binding(_, vs, f) =>
      vs.foreach(traverse(bound,_))
      traverse(bound ++ vs, f)
  }

  def traverse(f: Formula): Unit = traverse(Set(), f)

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

abstract class TransformerWithScope {

  def transform(bound: Set[Variable], f: Formula): Formula = f match {
    case l @ Literal(_) => l
    case v @ Variable(_) => v
    case f @ Application(fct, args) =>
      val args2 = args.map(transform(bound, _))
      Copier.Application(f, fct, args2)
    case b @ Binding(bt, vs, f) =>
      val bound2 = bound ++ vs
      val vs2 = vs.map(transform(bound2,_).asInstanceOf[Variable]) //this is bad but ...
      val f2 = transform(bound2, f)
      Copier.Binding(b, bt, vs2, f2)
  }
  
  def transform(f: Formula): Formula = transform(Set(), f)

}

class Mapper(fct: Formula => Formula) extends Transformer {
  override def transform(f: Formula): Formula = f match {
    case b @ Binding(bt, vs, f) =>
      def fct2(f: Formula): Formula = {
        if (vs.contains(f)) f else fct(f)
      }
      val m2 = new Mapper(fct2)
      fct(Copier.Binding(b, bt, vs, m2.transform(f)))
    case other =>
      fct(super.transform(f))
  }
}

class MapperWithScope(fct: (Set[Variable], Formula) => Formula) extends TransformerWithScope {
  override def transform(bound: Set[Variable], f: Formula): Formula = {
    fct(bound, super.transform(bound, f))
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

class TopDownMapper(fct: Formula => Formula) extends Transformer {
  override def transform(f: Formula): Formula = {
    val f2 = fct(f)
    if (f2 != f) {
      f2
    } else {
      f match {
        case b @ Binding(bt, vs, f) =>
          def fct2(f: Formula): Formula = {
            if (vs.contains(f)) f else fct(f)
          }
          val m2 = new TopDownMapper(fct2)
          Copier.Binding(b, bt, vs, m2.transform(f))
        case other =>
          super.transform(f)
      }
    }
  }
}

class StubornTopDownMapper(fct: Formula => Formula) extends Transformer {
  override def transform(f: Formula): Formula = {
    fct(f) match {
      case b @ Binding(bt, vs, f) =>
        def fct2(f: Formula): Formula = {
          if (vs.contains(f)) f else fct(f)
        }
        val m2 = new StubornTopDownMapper(fct2)
        Copier.Binding(b, bt, vs, m2.transform(f))
      case other =>
        super.transform(other)
    }
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

