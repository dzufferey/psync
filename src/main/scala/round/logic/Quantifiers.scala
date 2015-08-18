package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.{Namer, Misc}

object Quantifiers {

  /*  Sometime we introduce constant as shorthand for set.
   *  Negation makes them universal.
   *  This method fix this and put the ∃ back.
   *  assumes formula in PNF and flattened, looks only at the top level ∀
   */
  def fixUniquelyDefinedUniversal(f: Formula): Formula = f match {
    case ForAll(vs, f) =>
      val (_swappable, rest) = vs.partition(_.tpe match {case FSet(_) => true; case _ => false})
      val swappable = _swappable.toSet
      Logger("CL", Debug, "fix uniquely defined universal swappable: " + swappable.mkString(", "))
      //we are looking for clause like A = {x. ...} ⇒ ...
      val (prefix, f2) = FormulaUtils.getQuantifierPrefix(f)
      val avoid = f.boundVariables ++ vs

      def extractDef(candidates: Set[Variable], f: Formula): (Seq[(Variable, Formula)], Formula) = f match {
        case Or(lst @ _*) =>
          //try to get the definitions at this level
          val (_defs, rest) = lst.partition( f => f match {
            case Not(Eq(v @ Variable(_), c @ Comprehension(_,_))) if candidates(v) => true
            case Not(Eq(c @ Comprehension(_,_), v @ Variable(_))) if candidates(v) => true
            case _ => false
          })
          val defs1 = _defs.collect{
            case Not(Eq(v @ Variable(_), c @ Comprehension(_,_))) => v -> c
            case Not(Eq(c @ Comprehension(_,_), v @ Variable(_))) => v -> c
          }
          assert(defs1.size == _defs.size)
          
          //no def at this stage and present only in one clause, then can go further down
          val candidates2 = (candidates -- defs1.map(_._1)).filter( v => rest.filter(_.freeVariables contains v).size == 1 )
          val (defs2, rest2) = rest.map(extractDef(candidates2, _)).unzip

          (defs1 ++ defs2.flatten, Or(rest2:_*))

        case And(lst @ _*) =>
          //Conj: can go down on each clause separately
          val (ds, ls) = lst.map(extractDef(candidates, _)).unzip
          (ds.flatten, And(ls:_*))

        case other => (Seq.empty, other)
      }

      val (defs, f3) = extractDef(swappable, f2)

      Logger("CL", Debug, "fix uniquely defined universal defs: " + defs.mkString(", "))
      val eqs2 = defs.map{ case (a,b) => Eq(a, b) }
      val withDefs = And(f3 +: eqs2.toSeq :_*)
      val withPrefix = FormulaUtils.restoreQuantifierPrefix(prefix, withDefs)


      def hasDependencies(v: Variable, c: Formula) = {
        val deps = (c.freeVariables intersect avoid) - v
        deps.isEmpty
      }

      val swapped = defs.map(_._1).toSet
      Logger("CL", Info, "fix uniquely defined universal for: " + swapped.mkString(", "))
      val remaining = swappable.filterNot(swapped contains _).toList ::: rest
      val above = defs.flatMap{ case (v, c) => if ( hasDependencies(v,c)) Some(v) else None }.toList
      val below = defs.flatMap{ case (v, c) => if (!hasDependencies(v,c)) Some(v) else None }.toList
      val temp = Exists(above, ForAll(remaining, Exists(below, withPrefix)))
      val simp = Simplify.simplify(temp)
      simp

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
    val sk = skolemize(axiom)
    FormulaUtils.collectWithScope(true, check, sk)
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

  def hasFA(f: Formula) =
    FormulaUtils.exists({ case ForAll(_, _) => true; case _ => false }, f)

  def hasFAnotInComp(f: Formula): Boolean = f match {
    case Application(_, args) => args.exists(hasFAnotInComp)
    case ForAll(_, _) => true
    case Exists(_, f) => hasFAnotInComp(f)
    case _ => false
  }

}
