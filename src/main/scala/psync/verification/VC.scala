package psync.verification

import Utils._

import dzufferey.report._
import psync.formula._
import psync.logic._
import psync.utils.smtlib._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer

abstract class VC {

  protected var solved = false
  protected var status: Result = Unknown 

  def solve: Unit

  def isValid: Boolean = {
    if (!solved) solve
    status match {
      case UnSat => true
      case _ => false
    }
  }

  def decompose: VC

  def report: Item
}

class SingleVC( description: String,
                hypothesis: Formula,
                transition: Formula,
                conclusion: Formula,
                additionalAxioms: scala.List[Formula] = Nil,
                cl: CL = new CL(ClDefault)
              ) extends VC {

  protected lazy val fName = {
    Namer(description.replaceAll(" ", "_")) + ".smt2"
  }

  protected var reduced: Formula = null

  def solve {
    if (!solved) { 
      var solver: Solver = null
      try {
        Logger("VC", Notice, "solving: " + description)
        Logger("vC", Debug, "hypothesis:\n  " + FormulaUtils.getConjuncts(hypothesis).mkString("\n  "))
        Logger("vC", Debug, "transition:\n  " + FormulaUtils.getConjuncts(transition).mkString("\n  "))
        Logger("VC", Debug, "conclusion:\n  " + FormulaUtils.getConjuncts(conclusion).mkString("\n  "))
        Logger("VC", Debug, "additionalAxioms:\n  " + additionalAxioms.mkString("\n  "))
        val h0 = Simplify.simplify( And(hypothesis, transition, And(additionalAxioms:_*)) )
        val c0 = Simplify.simplify( conclusion )
        reduced = cl.entailment(h0, c0)
        solver = if (VerificationOptions.dumpVcs) Solver(UFLIA, fName)
                 else Solver(UFLIA)
        status = solver.testWithModel(reduced)
        solved = true
      } catch { case e: Exception =>
        status = Failure("Exception: " + e.getMessage + "\n  " + e.getStackTrace.mkString("\n  "))
        solved = true
      } finally {
      //if (solver != null)
      //  solver.exit
      }
      Logger("VC", Notice, "solved: " + description + " → " + status)
    }
  }

  def decompose = {
    // split ∨ in the hypothesis and the transition
    val splitHyp = FormulaUtils.getDisjuncts(hypothesis)
    val splitTrs = FormulaUtils.getDisjuncts(transition)
    // split ∧ in the conclusion
    val splitCon = FormulaUtils.getConjuncts(Simplify.splitForall(conclusion))
    // all the cases
    if (splitHyp.length * splitTrs.length * splitCon.length > 1) {
      var count = 0
      def mkVC(h: Formula, t: Formula, c: Formula) = {
        count += 1
        val descr = description + " Case " + count
        new SingleVC(descr, h, t, c, additionalAxioms, cl)
      }
      val vcs = for (h <- splitHyp; t <- splitTrs; c <- splitCon)
                yield mkVC(h,t,c)
      new CompositeVC(description, true, vcs)
    } else {
      this
    }
  }

  def report = {
    val stat = if (isValid) " (success)" else " (failed)"
    val lst = new Sequence(description + stat)
    lst.add(itemForFormula("Hypothesis", hypothesis))
    lst.add(itemForFormula("Transition", transition))
    lst.add(itemForFormula("Conclusion", conclusion))
    if (reduced != null) lst.add(itemForFormula("Reduced formula", reduced))
    status match {
      case Sat(Some(model)) => lst.add(new PreformattedText("Model", model.toString))
      case Unknown => lst.add(new PreformattedText("Reason", "solver returned unknown"))
      case Failure(reason) => lst.add(new PreformattedText("Reason", reason))
      case _ => 
    }
    lst
  }

}

class CompositeVC(description: String, all: Boolean, vcs: Seq[VC]) extends VC {

  def solve {
    if (!solved) {
      Logger("VC", Notice, "solving: " + description)
      //vcs.par.foreach(_.solve)
      vcs.foreach(_.solve)
      if ((all && vcs.forall(_.isValid)) ||
          vcs.exists(_.isValid)) {
        status = UnSat
      } else {
        status = Sat()
      }
      solved = true
    }
  }

  def decompose = new CompositeVC(description, all, vcs.map(_.decompose))

  def report = {
    val stat = if (isValid) " (success)" else " (failed)"
    val howMany = if (all) " [∀]" else " [∃]"
    val lst = new Sequence(description + howMany + stat)
    vcs.foreach( vc => lst.add(vc.report))
    lst
  }
}
