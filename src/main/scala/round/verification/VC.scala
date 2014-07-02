package round.verification

import round.formula._

class VC(description: String, hypothesis: Formula, transition: Formula, conclusion: Formula) {

  def mkSatQuery: Formula = And(And(hypothesis, transition), Not(conclusion))

  def isValid: Boolean = sys.error("TODO")

}
