package round.logic

import round.formula._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object CL {
  
  val procType = UnInterpreted("ProcessID")

  protected def normalize(f: Formula) = {
    //TODO some CNF conversion ?
    //TODO purification before or after instantiation ?
    val f1 = Simplify.normalize(f)
    val f2 = Simplify.nnf(f1)
    val f3 = Simplify.boundVarUnique(f2)
    f3
  }

  //unsupported quantifiers are implicitely universal, we care about the ∀ in ∀∃φ
  //TODO should still allows EPR no quantified processID below
  protected def getUnsupportedQuantifierPrefix(f: Formula): (Formula, List[Variable]) = {
    val f2 = Simplify.pnf(f)
    val (f3,vs) = Quantifiers.getUniversalPrefix(f2)
    val (supported, unsupported) = vs.toList.partition( v => v.tpe == procType)
    Logger("CL", Info, "unsupported quantifiers are: " + unsupported.map(v => v.name + ": " + v.tpe).mkString(", "))
    val f4 = ForAll(supported, f3)
    (f4, unsupported)
  }


  protected def matchQuantifiers( qf: Formula, 
                                  vs: List[Variable],
                                  existentials: Iterable[Variable],
                                  groundTerms: Iterable[Formula]
                                ): Iterable[Formula] = {

    def findMatch(v: Variable) = {
      val prefered = existentials.filter(_.tpe == v.tpe)
      if (!prefered.isEmpty) {
        prefered
      } else {
        Logger("CL", Notice, "did not find instantiation candidate among existential for " + v + ": " + v.tpe)
        groundTerms.filter(_.tpe == v.tpe)
      }
    }

    vs.foldLeft(List(qf))( (acc, v) => {
      val candidates = findMatch(v)
      if (candidates.isEmpty) {
        Logger("CL", Notice, "did not find any instantiation candidate for " + v + ": " + v.tpe)
        acc.map( f => ForAll(List(v), f) )
      } else {
        acc.flatMap( f => Quantifiers.instantiateWithTerms(v, f, candidates.toSet) )
      }
    })
  }

  /** preprocess and reduce (hypothesis ∧ ¬conclusion),
   *  returned formula can be checked for satisfiability. */
  def entailment(hypothesis: Formula, conclusion: Formula): Formula = {
    val h1 = normalize(hypothesis)
    val c1 = normalize(Not(conclusion))

    val (h2, ext) = Quantifiers.getExistentialPrefix(h1)
    val gt = FormulaUtils.collectGroundTerms(h2)
    //what can be used to instantiate the unsupported quantifiers: ext ∪ gt

    val cs1 = FormulaUtils.getConjuncts(c1)
    val cs2 = cs1.flatMap( c => {
      val (qf, vs) = getUnsupportedQuantifierPrefix(c)
      matchQuantifiers(qf, vs, ext, gt)
    })
    val cs3 = cs2.map(Quantifiers.fixUniquelyDefinedUniversal)

    val query = cs3.foldLeft(h2)(And(_,_))
    reduce(query)
  }
  
  def reduce(formula: Formula): Formula = {
    val n1 = normalize(formula)
    val n2 = Quantifiers.getExistentialPrefix(n1)._1
    val conjuncts = FormulaUtils.getConjuncts(n2)
    Logger("CL", Warning, "TODO reduce:\n  " + conjuncts.mkString("\n  "))
    False()
  }
  
}
