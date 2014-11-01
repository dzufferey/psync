package round.verification

import Utils._

import dzufferey.report._
import round.formula._
import round.logic._
import round.utils.smtlib._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer

class VC(description: String, hypothesis: Formula, transition: Formula, conclusion: Formula) {

  protected var solved = false
  protected var status: Result = Unknown 
  protected var reduced: Formula = False()

  protected lazy val fName = {
    Namer(description.replaceAll(" ", "_")) + ".smt2"
  }


  //TODO update to get the model if sat.
  def solve {
    try {
      Logger("VC", Notice, "solving: " + description)
      Logger("vC", Debug, "hypothesis:\n  " + FormulaUtils.getConjuncts(hypothesis).mkString("\n  "))
      Logger("vC", Debug, "transition:\n  " + FormulaUtils.getConjuncts(transition).mkString("\n  "))
      Logger("VC", Debug, "conclusion:\n  " + FormulaUtils.getConjuncts(conclusion).mkString("\n  "))
      reduced = CL.entailment(And(hypothesis, transition), conclusion)
      val solver = if (round.utils.Options.dumpVcs) Solver(UFLIA, fName)
                   else Solver(UFLIA)
      status = solver.testWithModel(reduced)
      solved = true
    } catch { case e: Exception =>
      status = Failure("Exception: " + e.getMessage + "\n  " + e.getStackTrace.mkString("\n  "))
      solved = true
    }
  }

  def isValid: Boolean = {
    if (!solved) solve
    status match {
      case UnSat => true
      case _ => false
    }
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
      case Unknown => lst.add(new PreformattedText("Reason", "solver returned unkown"))
      case Failure(reason) => lst.add(new PreformattedText("Reason", reason))
      case _ => 
    }
    lst
  }

}
