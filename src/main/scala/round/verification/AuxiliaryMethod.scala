package round.verification

import round.formula._

class AuxiliaryMethod(name: String,
                      params: List[Variable],
                      tpe: Function, tParams: List[TypeVariable],
                      pre: Formula,
                      body: TransitionRelation,
                      post: (Variable, Formula) ) {

  def makePreVC(args: List[Formula]): Formula = {
    sys.error("TODO")
  }

  def makePostAssume(args: List[Formula], ret: Formula): Formula = {
    sys.error("TODO")
  }

}
