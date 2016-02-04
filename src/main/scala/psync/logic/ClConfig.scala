package psync.logic

import psync.formula._
import psync.logic.quantifiers._

sealed abstract class QStrategy
case class QNew(tactic: Tactic, bound: Option[Int], local: Boolean) extends QStrategy

//TODO refactor to have a Generator instead of a QStrategy
case class ClConfig(vennBound: Option[Int],
                    onType: Option[Set[Type]],
                    instantiationStrategy: QStrategy)
{
}

object ClDefault extends ClConfig(
    Some(2),                        //pairwise Venn regions
    None,                           //on set of any type
    QNew(new Eager, Some(1), true)  //one step eager quantifier instantiation + local step
)

object ClFull extends ClConfig(
    None,                           //all Venn regions
    None,                           //on set of any type
    QNew(new Eager, Some(10), true) //one step eager quantifier instantiation + local step
)

object ClProc extends ClConfig(
    Some(2),                        //pairwise Venn regions
    Some(Set(CL.procType)),         //on set of type ProcessID only
    QNew(new Eager, Some(1), true)  //one step eager quantifier instantiation + local step
)

