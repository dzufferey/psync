package psync.logic

import psync.formula._
import psync.formula.InlineOps._
import dzufferey.utils.Logger
import TestCommon._

import org.scalatest._




//port of the example from the vmcai paper.
//they are more readable than dumping the VCs from the code
class Replay extends FunSuite {


  val nOver2 = Divides(n, Literal(2))

  val pld = UnInterpreted("payload")
  //a special type for the phase (try to reduce the blow-up)
  val phase = CL.timeType
  val r  = Variable("r").setType(phase)
  val r1 = Variable("r1").setType(phase)


  val a = Variable("A").setType(FSet(pid))
  val b = Variable("B").setType(FSet(pid))

  val s = Variable("S").setType(FSet(pid))

  val leader = Variable("leader").setType(pid)
  val p = Variable("p").setType(pid)

  val v = Variable("v").setType(pld)
  val t = Variable("t").setType(phase)

  val coord = UnInterpretedFct("coord", Some(pid ~> pid))


  val decided = UnInterpretedFct("decided", Some(pid ~> Bool))
  val decided1 = UnInterpretedFct("decided1", Some(pid ~> Bool))


  val commit = UnInterpretedFct("commit",Some(pid ~> Bool))
  val commit1 = UnInterpretedFct("commit1",Some(pid ~> Bool))

  val ready = UnInterpretedFct("ready",Some(pid ~> Bool))
  val ready1 = UnInterpretedFct("ready1",Some(pid ~> Bool))


  val epoch = UnInterpretedFct("epoch",Some(pid ~> Int))
  val epoch1 = UnInterpretedFct("epoch1",Some(pid ~> Int))

  val jump = UnInterpretedFct("jump",Some(pid ~> Bool))
  val jump1 = UnInterpretedFct("jump1",Some(pid ~> Bool))

  val fjump = UnInterpretedFct("fjump",Some(pid ~> Bool))
  val fjump1 = UnInterpretedFct("fjump1",Some(pid ~> Bool))


  def majorityS(f: Formula) = Lt(n, Times(Literal(2), Cardinality(f)))



    val primeMap = Map[Symbol,Symbol](
    //  data -> data1,
      decided -> decided1,
      commit -> commit1,
      ready -> ready1,
      epoch -> epoch1,
      jump -> jump1,
      fjump -> fjump1
    )
    def prime(f: Formula) = {
      val f1 = FormulaUtils.mapSymbol( x => primeMap.getOrElse(x, x), f)
      FormulaUtils.replace(r, r1, f1)
    }

    val round1a = And(
      //update
      // then branch + mailbox
      ForAll(List(i,j),
        Implies(And (Eq(i, coord(i)),
                     majorityS(ho(i)),
                     In(j,ho(i))),
                And(Eq(coord(j),i),
                    Eq(ready1(i), True())
                  )
            )
      ),
      ForAll(List(i),
        Implies(Not(And (Eq(i, coord(i)),
                     majorityS(ho(i)),
                     )),
                     Eq(ready1(i),False()))
                   )
    )

    val propoutro =  Exists(List(leader), And(
         Eq(s,Comprehension(List(j), Eq(coord(j),leader))),
         majorityS(s)))

    val propintro = ForAll(List(i),Eq(ready(i),False()))

    //specific tactic
    def conf(e: Int = 1) = {
      import psync.logic.quantifiers._
      val local = true
      val vennBound = 2
      val tactic = new Sequence(
          new Eager(Map[Type,Int](pid -> 1, FSet(pid) -> 1, phase -> 1, pld -> 1).withDefaultValue(0)),
          new Eager(e)
        )
      ClConfig(Some(vennBound), None, QStrategy(tactic, local))
    }


    //test VCs

     test("round one if update condition "){
       val fs = List(propintro,
                round1a,
                Or(Not(propoutro),prime(propintro)))
         assertSat(fs, c2e2)
         getModel(fs)
     }

}
