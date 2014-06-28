package round.verification

import round.formula._

//a wrapper around a formula, old/primed variables, ...

class TransitionRelation(tr: Formula, old: List[Variable], primed: List[Variable]) {
  //TODO
  //-renaming facility
  //-check for capture of intermediate variable
  //-generating precondition check for call
  //-replacing call by postcondition ...
  //-...
}

object TransitionRelation {

  val procType = UnInterpreted("Process")

  val mailboxSymbol: Symbol = {
    UnInterpretedFct("__mailbox__", Some(Function(List(procType, procType), Bool)))
  }

  def mailbox(i: Formula, j: Formula) = Application(mailboxSymbol, List(i,j))

}
