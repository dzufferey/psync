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

  /*  Sometime we introduce constant as shorthand for set.
   *  Negation makes them universal.
   *  This method fix this and put the ∃ back.
   *  assumes formula in PNF, looks only at the top level ∀
   */
  def fixUniquelyDefinedUniversal(f: Formula): Formula = f match {
    case ForAll(vs, f) =>
      val (swappable, rest) = vs.partition(_.tpe match {case FSet(_) => true; case _ => false})
      Logger("CL", Debug, "fix uniquely defined universal swappable: " + swappable.mkString(", "))

      //we are looking for clause like A = {x. ...} ⇒ ...
      val (prefix, f2) = FormulaUtils.getQuantifierPrefix(f)
      val avoid = f.boundVariables ++ vs
      val disj = FormulaUtils.getDisjuncts(f2)
      Logger("CL", Debug, "fix uniquely defined universal disj:\n " + disj.mkString("\n "))
      def interesting(v: Variable, f: Formula): Boolean = (swappable contains v)
      val (_defs, restf) = disj.partition( f => f match {
        case Not(Eq(v @ Variable(_), c @ Comprehension(_,_))) if interesting(v, c) => true
        case Not(Eq(c @ Comprehension(_,_), v @ Variable(_))) if interesting(v, c) => true
        case _ => false
      })
      val eqs = _defs.collect{ case Not(eq) => eq }
      assert(eqs.size == _defs.size)
      def checkDependencies(v: Variable, c: Formula): Formula = {
        val deps = (f.freeVariables intersect avoid) - v
        skolemify(v, deps)
      }
      val defs = eqs.map{
        case Eq(v @ Variable(_), c @ Comprehension(_,_)) =>
          v -> (checkDependencies(v,c), c)
        case Eq(c @ Comprehension(_,_), v @ Variable(_)) =>
          v -> (checkDependencies(v,c), c)
        case _ => ???
      }.toMap
      Logger("CL", Debug, "fix uniquely defined universal defs: " + defs.mkString(", "))
      def subst(f: Formula) = f match {
        case v @ Variable(_) if defs contains v => defs(v)._1
        case other => other
      }
      val eqs2 = defs.values.map{ case (a,b) => Eq(a, b) }
      val swapped = defs.keySet
      val restf2 = restf.foldLeft(False(): Formula)(Or(_, _))
      val restf3 = FormulaUtils.map(subst, restf2)
      val withDefs = eqs2.foldLeft(restf3)(And(_, _))
      val withPrefix = FormulaUtils.restoreQuantifierPrefix(prefix, withDefs)
      val remaining = swappable.filterNot(swapped contains _) ::: rest
      Logger("CL", Info, "fix uniquely defined universal for: " + swapped.mkString(", "))
      ForAll(remaining, withPrefix)
    case other => other
  }

  def isEPR(axiom: Formula): Boolean = {
    def check(acc: Boolean, vs: Set[Variable], f: Formula) = f match {
      case Application(_, args) =>
        val hasV = args.exists{
          case v @ Variable(_) => vs contains v
          case _ => false
        }
        acc && !(hasV && f.tpe != Bool)
      case _ =>
        acc
    }
    FormulaUtils.collectWithScope(true, check, axiom)
  }

  def isStratified(axiom: Formula, lt: (Type, Type) => Boolean): Boolean = {
    def isGround(vs: Set[Variable], f: Formula) = f.freeVariables.intersect(vs).isEmpty
    def check(acc: Boolean, vs: Set[Variable], f: Formula) = f match {
      case Application(_, args) =>
        acc && (f.tpe == Bool || args.forall( a => isGround(vs, a) || lt(f.tpe, a.tpe) ) )
      case _ =>
        acc
    }
    FormulaUtils.collectWithScope(true, check, axiom)
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
    
  /** create a skolem constant corresponding to a variable */
  def skolemify(v: Variable, bound: Set[Variable]) = {
    if (bound.isEmpty) {
      v
    } else {
      val args = bound.toList
      val fct = UnInterpretedFct(v.name, Some(Function(args.map(_.tpe), v.tpe)))
      Copier.Application(v, fct, args)
    }
  }

  /** replace ∃ below ∀ by skolem functions.
   *  assumes the bound var have unique names. */
  def skolemize(f: Formula): Formula = {

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
