package psync.logic

import psync.formula._
import psync.formula.InlineOps._
import dzufferey.utils.Logger
import TestCommon._

import org.scalatest._

class VsExample extends FunSuite {

  // Defs

  val key = Int
  val pld = UnInterpreted("payload")

  val coord = Variable("coord").setType(pid)
  val mailbox = UnInterpretedFct("mailbox", Some(pid ~> FMap(pid, pld)))
  val idx = Variable("idx").setType(key)

  val li0 = Variable("li0").setType(key)
  val li1 = Variable("li1").setType(key)
  val logValue0 = UnInterpretedFct("logValue0", Some(pid ~> FMap(key, pld)))
  val logValue1 = UnInterpretedFct("logValue1", Some(pid ~> FMap(key, pld)))
  val logCommit0 = UnInterpretedFct("logCommit0", Some(pid ~> FMap(key, Bool)))
  val logCommit1 = UnInterpretedFct("logCommit1", Some(pid ~> FMap(key, Bool)))
  val act0 = Variable("Act0").setType(FSet(pid))
  val act1 = Variable("Act1").setType(FSet(pid))

  val primeMapS = Map[Symbol,Symbol](
    logValue0 -> logValue1,
    logCommit0 -> logCommit1
  )
  val primeMapV = Map[Variable, Variable](
    li0 -> li1,
    act0 -> act1
  )
  def prime(f: Formula) = {
    val f1 = FormulaUtils.mapSymbol( x => primeMapS.getOrElse(x, x), f)
    FormulaUtils.alpha(primeMapV, f1)
  }


  // Spec
  val inv0 = And(
    //the log is split into separate log for commits and value
    ForAll(List(i), logValue0(i).keySet === logCommit0(i).keySet),
    ForAll(List(i, idx), logCommit0(i).isDefinedAt(idx) ==> And(idx ≤ li0, idx ≥ 1)),
    ForAll(List(i), logValue0(i).size ≤ li0)
  )

  val inv1 = And(
    logCommit0(coord).isDefinedAt(li0-1),
    logCommit0(coord).lookUp(li0-1),
    act0.card > (n/2),
    ForAll(List(i), (i ∈ act0) ==> And(
      logValue0(i).lookUp(li0-1) === logValue0(coord).lookUp(li0-1),
      Not(logCommit0(i).lookUp(li0-1))
    ))
  )

  val inv2 = {
    And(
      Comprehension(List(i), And(
        logCommit0(i).size === logCommit0(coord).size,
        Not(logCommit0(i).lookUp(li0)),
        i ∈ act0
      )).card ≥ (n/2)
    )
  }

  // Rounds

  val r1 = {
    val sendCond = And(
      i ∈ act0,
      i === coord,
      logValue0(i).isDefinedAt(li0)
    )
    val send = And(
      ForAll(List(i, j), sendCond ==> And(mailbox(j).isDefinedAt(i), mailbox(j).lookUp(i) === logValue0(i).lookUp(li0))),
      ForAll(List(i, j), Not(sendCond) ==> Not(mailbox(j).isDefinedAt(i)))
    )
    val updateCondA = And(i ∈ act0, mailbox(i).isDefinedAt(coord))
    val updateCondB = And(li0 > 0, logValue0(i).isDefinedAt(li0 - 1))
    val update = And(
      li1 === li0,
      ForAll(List(i), updateCondA ==> And(
        i ∈ act1,
        logValue1(i) === logValue0(i).updated(li0, mailbox(i).lookUp(coord)),
        updateCondB ==> (logCommit1(i) === logCommit0(i).updated(li0, False()).updated(li0-1, True())),
        Not(updateCondB) ==> (logCommit1(i) === logCommit0(i).updated(li0, False()))
      )),
      ForAll(List(i), Not(updateCondA) ==> And(
        Not(i ∈ act1),
        logValue1(i) === logValue0(i),
        logCommit1(i) === logCommit0(i)
      ))
    )
    send ∧ update
  }

  test("Sanity check 1") {
    assertSat(List(inv0, inv1, inv2))
  }

  test("Sanity check 2") {
    assertUnsat(List(inv0, Not(inv0)))
  }
  
  test("Sanity check 3") {
    assertUnsat(List(inv1, Not(inv1)))
  }

  test("Sanity check 4") {
    assertUnsat(List(inv2, Not(inv2)))
  }
  
  test("Sanity check 5") {
    val i = And(inv0, inv1, inv2)
    assertUnsat(List(i, Not(i)))
  }
  
  test("Sanity check 6") {
    assertSat(List(r1), to = 60000)
  }

  ignore("More complex example with maps...") {
    ???
  }

}
