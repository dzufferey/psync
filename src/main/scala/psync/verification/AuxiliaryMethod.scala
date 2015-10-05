package psync.verification

import Utils._

import psync.formula._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

class AuxiliaryMethod(val name: String,
                      val params: List[Variable],
                      val tpe: Function,
                      val tParams: List[TypeVariable],
                      val pre: Formula,
                      val body: Option[TransitionRelation],
                      val post: Option[(Variable, Formula)] ) {

  //TODO check the old vars in body are the same as the args
  //TODO instantiate the types went generating VCs

  def applyType(tps: List[Type]): AuxiliaryMethod = {
    if (tps == Nil) {
      this
    } else {
      assert(tps.length == tParams.length)
      Logger("AuxiliaryMethod", Warning, "TODO applyType: " + name + tps.mkString("[",", ","]"))
      this
    }
  }

  def hasDef = body.isDefined

  def symbol = UnInterpretedFct(name, Some(tpe), tParams)

  private def argsSubst(args: List[Formula])(f: Formula) = {
    assert(params.length == args.length)
    val subst = params.zip(args).foldLeft(Map.empty[Formula, Formula])(_ + _)
    FormulaUtils.map(( x => subst.getOrElse(x, x)), f)
  }

  /* Generate the precondition of the call with 'args' as arguments. */
  def makePreVC(args: List[Formula]): Formula = {
    argsSubst(args)(pre)
  }

  /* Generate the postcondition of the call with 'args' as arguments and return value assigned to 'ret'. */
  def makePostAssume(args: List[Formula], ret: Formula): Option[Formula] = {
    post.map( post => {
      val post2 = argsSubst(args)(post._2)
      FormulaUtils.map(( x => if (x == post._1) ret else x), post2)
    })
  }

  def report = {
    import dzufferey.report._
    val tp = if (tParams == Nil) "" else tParams.mkString("[",",","]")
    val pr = params.zip(tpe.args).map{ case (p,t) => p.name + ": " + t }.mkString("(",",",")")
    val lst = new Sequence(name + tp + pr + ": " + tpe.returns)
    lst.add(itemForFormula("Precondition", pre))
    if (post.isDefined) {
        lst.add(itemForFormula("Postcondition("+post.get._1+")", post.get._2))
    } else {
        lst.add(new Text("Postcondition", "undefined"))
    }
    for (b <- body) lst.add(b.report)
    lst
  }

}
