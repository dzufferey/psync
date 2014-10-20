package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.{Namer, Misc}

object Quantifiers {

  //normal instantiation is just substitution
  //TODO groundTerms should be up to equalities / equivalence classes
  def instantiateWithTerms(v: Variable, axiom: Formula, groundTerms: Set[Formula], local: Boolean = false): List[Formula] = {
    if (local) {
      ??? //TODO local instantiation needs fetching the fct up to boolean level and implementing E-matching
    } else {
      val candidates = groundTerms.filter(_.tpe == v.tpe).toList
      candidates.toList.map( gt => FormulaUtils.map(x => if (x == v) gt else x, axiom) )
    }
  }

  def isEPR(axiom: Formula): Boolean = {
    ???
  }

  def isStratified(axiom: Formula, lt: (Type, Type) => Boolean): Boolean = {
    ???
  }

  def existentiallyQantified(f: Formula): Set[Variable] = {
    def process(acc: Set[Variable], f: Formula) = f match {
      case Exists(vs, _) => acc ++ vs
      case _ => acc
    }
    FormulaUtils.collect(Set[Variable](), process, f)
  }

  def universallyQantified(f: Formula): Set[Variable] = {
    def process(acc: Set[Variable], f: Formula) = f match {
      case ForAll(vs, _) => acc ++ vs
      case _ => acc
    }
    FormulaUtils.collect(Set[Variable](), process, f)
  }
  
  protected def getQuantPrefix(f: Formula, exists: Boolean): (Formula, List[Variable]) = {

    var introduced = List[Variable]()

    def renameVar(v: Variable, vs: Set[Variable]): Variable = {
      var v2 = v.name
      val taken = vs.map(_.name)
      while (taken contains v2) {
        v2 = Namer(v2)
      }
      val vv = Copier.Variable(v, v2)
      introduced = vv :: introduced
      vv
    }

    def handleQuant(vs: List[Variable], f: Formula, fv: Set[Variable]): (Formula, Set[Variable]) = {
      val subst = vs.map(v => (v -> renameVar(v, fv)) ).toMap
      val f2 = FormulaUtils.alpha(subst, f)
      val fv2 = fv ++ subst.values
      process(f2, fv2)
    }

    def process(f: Formula, fv: Set[Variable]): (Formula, Set[Variable]) = f match {
      case ForAll(vs, f2) =>
        if (!exists) handleQuant(vs, f2, fv)
        else (f, fv)
      case Exists(vs, f2) =>
        if (exists) handleQuant(vs, f2, fv)
        else (f, fv)
      case a @ Application(fct, args) =>
        val (args2, fv2) = Misc.mapFold(args, fv, process)
        (Copier.Application(a, fct, args2), fv2)
      case other => (other, fv)
    }

    (process(f, f.freeVariables)._1, introduced)
  }

  /** remove top level ∃, returns the new formula and the newly introduced variables */
  def getExistentialPrefix(f: Formula): (Formula, List[Variable]) = getQuantPrefix(f, true)

  /** remove top level ∀, returns the new formula and the newly introduced variables */
  def getUniversalPrefix(f: Formula): (Formula, List[Variable]) = getQuantPrefix(f, false)


  /** replace ∃ below ∀ by skolem functions.
   *  assumes the bound var have unique names. */
  def skolemize(f: Formula): Formula = {

    def skolemify(v: Variable, bound: Set[Variable]) = {
      if (bound.isEmpty) {
        v
      } else {
        val args = bound.toList
        val fct = UnInterpretedFct(v.name, Some(Function(args.map(_.tpe), v.tpe)))
        Copier.Application(v, fct, args)
      }
    }

    def process(bound: Set[Variable], f: Formula): Formula = f match {
      case l @ Literal(_) => l
      case v @ Variable(_) => v
      case a @ Application(fct, args) =>
        Copier.Application(a, fct, args.map(process(bound, _)))
      case b @ Binding(bt, vs, f) =>
        bt match {
          case Comprehension => b
          case ForAll =>
            ForAll(vs, process(bound ++ vs, f))
          case Exists => 
            val map = vs.map( v => (v -> skolemify(v, bound)) ).toMap
            def fct(f: Formula) = f match {
              case v: Variable => map.getOrElse(v,v)
              case _ => f
            }
            val f2 = FormulaUtils.map(fct, f)
            process(bound, f2)
        }
    }

    process(Set(), f)
  }

}
