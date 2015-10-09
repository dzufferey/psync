package psync.logic

import psync.formula._

sealed abstract class QStrategy
case class Eager(bound: Option[Int], local: Boolean) extends QStrategy
case class Guided(bound: Option[Int], local: Boolean) extends QStrategy
case class QSeq(first: QStrategy, second: QStrategy) extends QStrategy

case class ClConfig(vennBound: Option[Int],
                    onType: Option[Set[Type]],
                    instantiationStrategy: QStrategy)
{
}

object ClDefault extends ClConfig(
    Some(2),                //pairwise Venn regions
    None,                   //on set of any type
    Eager(Some(1), true)    //one step eager quantifier instantiation + local step
)

object ClFull extends ClConfig(
    None,                   //all Venn regions
    None,                   //on set of any type
    Eager(Some(10), true)   //10 steps eager quantifier instantiation + local step
)

object ClProc extends ClConfig(
    Some(2),                //pairwise Venn regions
    Some(Set(CL.procType)), //on set of type ProcessID only
    Eager(Some(1), true)    //one step eager quantifier instantiation + local step
)

