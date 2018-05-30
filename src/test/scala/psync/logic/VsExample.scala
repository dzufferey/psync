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
  val log0 = UnInterpretedFct("log0", Some(pid ~> FMap(key, Product(pld, Bool))))
  val log1 = UnInterpretedFct("log1", Some(pid ~> FMap(key, Product(pld, Bool))))
  val act0 = Variable("Act0").setType(FSet(pid))
  val act1 = Variable("Act1").setType(FSet(pid))

  val primeMapS = Map[Symbol,Symbol](
    log0 -> log1
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
    ForAll(List(i, idx), log0(i).isDefinedAt(idx) ==> And(idx ≤ li0, idx ≥ 1)),
    ForAll(List(i), log0(i).size ≤ li0)
  )

  val inv1 = And(
    log0(coord).isDefinedAt(li0-1),
    log0(coord).lookUp(li0-1)._2,
    ForAll(List(i), (i ∈ act0) ==> (
      log0(i).lookUp(li0-1)._1 === log0(coord).lookUp(li0-1)._1
    ))
  )

  val inv2 = {
    And(
      Comprehension(List(i), And(
        log0(i).size === log0(coord).size,
        Not(log0(i).lookUp(li0)._2),
        i ∈ act0
      )).card ≥ (n/2)
    )
  }

  // Rounds

  val r1 = {
    val sendCond = And(
      i ∈ act0,
      i === coord,
      log0(i).isDefinedAt(li0)
    )
    val send = And(
      ForAll(List(i, j), sendCond ==> And(mailbox(j).isDefinedAt(i), mailbox(j).lookUp(i) === log0(i).lookUp(li0)._1)),
      ForAll(List(i, j), Not(sendCond) ==> Not(mailbox(j).isDefinedAt(i)))
    )
    val updateCondA = And(i ∈ act0, mailbox(i).isDefinedAt(coord))
    val updateCondB = Not(log0(i).lookUp(li0-1)._2)
    val update = And(
      li1 === li0,
      ForAll(List(i), updateCondA ==> And(
        i ∈ act1,
        updateCondB ==>
          (log1(i) === log0(i).updated(li0, Tuple(mailbox(i).lookUp(coord), False())).updated(li0-1, Tuple(log0(i).lookUp(li0-1)._1, True()))),
        Not(updateCondB) ==>
          (log1(i) === log0(i).updated(li0, Tuple(mailbox(i).lookUp(coord), False())))
      )),
      ForAll(List(i), Not(updateCondA) ==> And(
        Not(i ∈ act1),
        log1(i) === log0(i)
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

  test("Sanity check 7") {
    assertSat(List(r1, inv0, inv1, inv2),
              reducer = cln(1, new quantifiers.Eager(Some(1)), true),
              to = 60000)
  }

  //DZ: needs to look deeper in the following ...

  ignore("inv0 inductive") {
    assertUnsat(List(r1, inv0, inv1, inv2, Not(prime(inv0))),
                reducer = cln(1, new quantifiers.Eager(Some(2)), true),
                to = 60000,
                fname = Some("inv0.smt2"),
                debug = true)
  }

  ignore("inv1 inductive") {
    assertUnsat(List(r1, inv0, inv1, inv2, Not(prime(inv1))),
                reducer = cln(1, new quantifiers.Eager(Some(1)), true),
                to = 60000)
  }

  //need higher bound -> lots of memory...
  ignore("inv2 inductive") {
    assertUnsat(List(r1, inv0, inv1, inv2, Not(prime(inv2))),
                reducer = cln(2, new quantifiers.Eager(Some(1)), true),
                to = 60000)
  }

  test("check 0") {
    val log0 = UnInterpretedFct("log0", Some(pid ~> FMap(key, Product(Int, Bool))))
    val log1 = UnInterpretedFct("log1", Some(pid ~> FMap(key, Product(Int, Bool))))
    val f = And(
      log0(coord).isDefinedAt(li0-1),
      log0(coord).lookUp(li0-1)._2,
      log1(coord).isDefinedAt(li1-1),
      Not(log1(coord).lookUp(li1-1)._2),
      li0 === li1,
      log1(coord) === log0(coord).updated(li0, Tuple(1, False()))
    )
    assertUnsat(List(f))
  }

  test("check 1") {
    val newlog0 = UnInterpretedFct("log0", Some(pid ~> FMap(key, Bool)))
    val newlog1 = UnInterpretedFct("log1", Some(pid ~> FMap(key, Bool)))
    val f = And(
      newlog0(coord).isDefinedAt(li0-1),
      newlog0(coord).lookUp(li0-1),
      newlog1(coord).isDefinedAt(li1-1),
      Not(newlog1(coord).lookUp(li1-1)),
      li0 === li1,
      newlog1(coord) === newlog0(coord).updated(li0, False())
    )
    assertUnsat(List(f))
  }

}
