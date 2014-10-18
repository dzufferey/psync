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
  protected var status: Either[Boolean, String] = Left(false)

  protected lazy val fName = {
    Namer(description.replaceAll(" ", "_")) + ".smt"
  }

  def solve {
    try {
      Logger("VC", Info, "solving: " + description)
      val reduced = CL.entailment(And(hypothesis, transition), conclusion)
      val solver = if (round.utils.Options.dumpVcs) Solver(UFLIA, fName)
                   else Solver(UFLIA)
      solver.test(reduced) match {
        case Some(b) =>
          status = Left(b)
        case None =>
          status = Right("could not solve " + reduced)
      }
      solved = true
    } catch { case e: Exception =>
      status = Right("Exception: " + e.getMessage + "\n" + e.getStackTrace)
      solved = true
    }
  }

  def isValid: Boolean = {
    if (!solved) solve
    status match {
      case Left(true) => true
      case _ => false
    }
  }

  def report = {
    val stat = if (isValid) " (success)" else " (failed)"
    val lst = new Sequence(description + stat)
    lst.add(itemForFormula("Hypothesis", hypothesis))
    lst.add(itemForFormula("Transition", transition))
    lst.add(itemForFormula("Conclusion", conclusion))
    status match {
      case Right(reason) => lst.add(new PreformattedText("Reason", reason))
      case _ => 
    }
    lst
  }

}
