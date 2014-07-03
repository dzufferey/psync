package round.verification

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

  def isValid: Either[Boolean, String] = {
    if (!solved) solve
    status
  }

}
