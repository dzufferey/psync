package round.verification

import round.formula._

class AuxiliaryMethod(val name: String,
                      val params: List[Variable],
                      val tpe: Function,
                      val tParams: List[TypeVariable],
                      val pre: Formula,
                      val body: Option[TransitionRelation],
                      val post: (Variable, Formula) ) {


  def hasDef = body.isDefined

  def symbol = UnInterpretedFct(name, Some(tpe), tParams)

  def makePreVC(args: List[Formula]): Formula = {
    sys.error("TODO")
  }

  def makePostAssume(args: List[Formula], ret: Formula): Formula = {
    sys.error("TODO")
  }

}
