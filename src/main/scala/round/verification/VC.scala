package round.verification

import Utils._

import dzufferey.report._
import round.formula._
import round.utils.Logger
import round.utils.LogLevel._

class VC(description: String, hypothesis: Formula, transition: Formula, conclusion: Formula) {

  protected var solved = false
  protected var status: Either[Boolean, String] = Left(false)

  def solve {
    try {
      //TODO matching quantifier!
      val satQuery = And(And(hypothesis, transition), Not(conclusion))
      Logger("VC", Warning, "TODO: solve!!")
      status = Right("TODO")
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
