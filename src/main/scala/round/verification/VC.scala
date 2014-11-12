package round.verification

import Utils._

import dzufferey.report._
import round.formula._
import round.logic._
import round.utils.smtlib._
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

  def report: Item
}

class SingleVC(description: String, hypothesis: Formula, transition: Formula, conclusion: Formula, additionalAxioms: scala.List[Formula] = Nil) extends VC {

  protected lazy val fName = {
    Namer(description.replaceAll(" ", "_")) + ".smt2"
  }

  protected var reduced: Formula = False()

  def solve {
    var solver: Solver = null
    try {
      Logger("VC", Notice, "solving: " + description)
      Logger("vC", Debug, "hypothesis:\n  " + FormulaUtils.getConjuncts(hypothesis).mkString("\n  "))
      Logger("vC", Debug, "transition:\n  " + FormulaUtils.getConjuncts(transition).mkString("\n  "))
      Logger("VC", Debug, "conclusion:\n  " + FormulaUtils.getConjuncts(conclusion).mkString("\n  "))
      Logger("VC", Debug, "additionalAxioms:\n  " + additionalAxioms.mkString("\n  "))
      reduced = CL.entailment(And(hypothesis, transition), conclusion)
      reduced = Application(And, FormulaUtils.getConjuncts(reduced) ::: additionalAxioms).setType(Bool)
      reduced = Simplify.simplify(reduced)
      solver = if (round.utils.Options.dumpVcs) Solver(UFLIA, fName)
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

  def report = {
    val stat = if (isValid) " (success)" else " (failed)"
    val lst = new Sequence(description + stat)
    lst.add(itemForFormula("Hypothesis", hypothesis))
    lst.add(itemForFormula("Transition", transition))
    lst.add(itemForFormula("Conclusion", conclusion))
    lst.add(itemForFormula("Reduced formula", reduced))
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
    Logger("VC", Notice, "solving: " + description)
    //vcs.par.foreach(_.solve)
    vcs.foreach(_.solve)
    if ((all && vcs.forall(_.isValid)) ||
        vcs.exists(_.isValid)) {
      status = UnSat
    } else {
      status = Sat()
    }
  }

  def report = {
    val stat = if (isValid) " (success)" else " (failed)"
    val howMany = if (all) " [∀]" else " [∃]"
    val lst = new Sequence(description + howMany + stat)
    vcs.foreach( vc => lst.add(vc.report))
    lst
  }
}
