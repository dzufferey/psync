package round.logic

import round.formula._
import round.formula.InlineOps._
import TestCommon._

import org.scalatest._

//port of the example from the vmcai paper.
//they are more readable than dumping the VCs from the code
class OtrExample extends FunSuite {
    
  //for a more expensive version we can replace payload by Int and leq by Leq
  val pld = UnInterpreted("payload")
  val leq = UnInterpretedFct("leq", Some(pld ~> pld ~> Bool))

  val a = Variable("A").setType(FSet(pid))

  val v = Variable("v").setType(pld) 

  val mailbox = UnInterpretedFct("mailbox", Some(pid ~> FMap(pid, pld)))

  val data = UnInterpretedFct("data",Some(pid ~> pld))
  val data1 = UnInterpretedFct("data1",Some(pid ~> pld))
  val data0 = UnInterpretedFct("data0",Some(pid ~> pld))
  val decided = UnInterpretedFct("decided", Some(pid ~> Bool))
  val decided1 = UnInterpretedFct("decided1", Some(pid ~> Bool))

  val primeMap = Map[Symbol,Symbol](
    data -> data1,
    decided -> decided1
  )
  def prime(f: Formula) = FormulaUtils.mapSymbol( x => primeMap.getOrElse(x, x), f)

  val agreement = ForAll(List(i,j), Implies(And(decided(i), decided(j)), Eq(data(i),data(j))))
  val integrity = ForAll(List(i), Implies(decided(i), And(decided1(i), Eq(data(i), data1(i)))))
  val termination = ForAll(List(i), decided(i))
  val validity = ForAll(List(i), Exists(List(j), Eq(data(i), data0(j))))
  val validityPrimed = ForAll(List(i), Exists(List(j), Eq(data1(i), data0(j))))

  def twoThird(f: Formula)    = Gt(Cardinality(f), Divides(Times(IntLit(2), n), IntLit(3)))
  def twoThirdMap(f: Formula) = Gt(Size(f),        Divides(Times(IntLit(2), n), IntLit(3)))


  val invariantAgreement = Or(
    ForAll(List(i), Not(decided(i))),
    Exists(List(v,a), And(
      Eq(a, Comprehension(List(i), Eq(data(i), v))),
      twoThird(a),
      ForAll(List(i), Implies(decided(i), Eq(data(i), v)))
    ))
  )

  val invariantProgress1 = Exists(List(v,a), And(
    Eq(a, Comprehension(List(i), Eq(data(i), v))),
    Eq(Cardinality(a), n),
    ForAll(List(i), Implies(decided(i), Eq(data(i), v)))
  ))
  
  val invariantProgress2 =
    Exists(List(v), ForAll(List(i), And(decided(i), Eq(data(i), v))))

  val initialState = ForAll(List(i), Eq(decided(i), False()))

  //min most often received
  val mmor = UnInterpretedFct("mmor", Some(pid ~> pld))

  def valueIs(f: Formula) = Comprehension(List(j), And( IsDefinedAt(mailbox(i), j),
                                                        Eq(LookUp(mailbox(i), j), f)))

  val defs = {
    val pld1 = Variable("pld1").setType(pld)
    val pld2 = Variable("pld2").setType(pld)
    val pld3 = Variable("pld3").setType(pld)
    And(
      //leq is a total order
      ForAll(List(pld1, pld2), And(
        Or(leq(pld1, pld2), leq(pld2, pld1)),
        Implies(leq(pld1, pld2) && leq(pld2, pld1), pld1 === pld2)
      )),
      ForAll(List(pld1, pld2, pld3),
        Implies(leq(pld1, pld2) && leq(pld2, pld3), leq(pld1, pld3))
      ),
      //mmor def 
      ForAll(List(i,pld1), And(
        valueIs(pld1).card <= valueIs(mmor(i)).card,
        Implies( valueIs(pld1).card === valueIs(mmor(i)).card, leq(mmor(i), pld1))
      ))
    )
  }

  val tr = And(
    defs,
    //send, mailbox
    ForAll(List(i),   Eq(KeySet(mailbox(i)),    ho(i))),
    ForAll(List(i,j), Eq(LookUp(mailbox(i), j), data(j))),
    //update
    ForAll(List(i), And(
      Implies(twoThirdMap(mailbox(i)),
        And(Eq(data1(i), mmor(i)),
            Exists(List(a), And(
              Eq(a, valueIs(mmor(i))),
              Implies(twoThird(a), Eq(decided1(i), True())),
              Implies(Not(twoThird(a)), Eq(decided1(i), decided(i)))
        )))
      ),
      Implies(Not(twoThirdMap(mailbox(i))),
        And(Eq(decided(i), decided1(i)), Eq(data1(i), data(i)))
      )
    ))
  )

  val magicRound = Exists(List(a), And(
    Lt(Times(n, Literal(2)), Times(Cardinality(a), Literal(3))),
    ForAll(List(i), Eq(ho(i), a))
  ))
  
  test("initial state implies invariant") {
    assertUnsat(List(initialState, Not(invariantAgreement)))
  }

  test("invariant implies agreement") {
    assertUnsat(List(invariantAgreement, Not(agreement)))
  }
  
  test("invariant implies termination") {
    assertUnsat(List(invariantProgress2, Not(termination)))
  }

  test("validity holds initially") {
    assertUnsat(List(ForAll(List(i), Eq(data0(i), data(i))), Not(validity)))
  }

  test("mmor unsat") {
    val fs = List(
      defs,
      //send
      ForAll(List(i,j), And(
        IsDefinedAt(mailbox(i), j) === ho(i).contains(j),
        LookUp(mailbox(i), j) === data(j)
      )),
      //env assumptions
      Comprehension(List(i), data(i) === v).card > ((n * 2) / 3),
      ForAll(List(i), ho(i).card > ((n * 2) / 3) ),
      //negated prop: ¬(∀ k. mmor(k) == v)
      mmor(k) !== v
    )
    assertUnsat(fs, 10000, false, cl3_1)
  }
  
//test("invariant is inductive") {
//  val fs = List(
//    //ForAll(List(i), Eq(Size(mailbox(i)),Literal(0))), //try the else branch
//    invariantAgreement,
//    tr,
//    Not(prime(invariantAgreement))
//  )
//  //assertUnsat(fs, 10000, false, cl3_2)
//  assertUnsat(fs, 60000, true, cl3_3, Some("test3_3.smt2"))
//  //assertUnsat(fs, 60000, true, cl3_2, Some("test_mf.smt2"), true)
//}

//test("1st magic round") {
//  val fs = List(
//    invariantAgreement,
//    magicRound
//    tr,
//    Not(prime(invariantProgress1))
//  )
//  assertUnsat(fs)
//}

//test("invariant 1 is inductive") {
//  val fs = List(
//    invariantProgress1,
//    tr,
//    Not(prime(invariantProgress1))
//  )
//  //assertUnsat(fs, 30000, false, cl3_2)
//  getModel(fs, 30000, cl3_2)
//}

//test("2nd magic round") {
//  val fs = List(
//    invariantProgress1,
//    magicRound
//    tr,
//    Not(prime(invariantProgress2))
//  )
//  assertUnsat(fs)
//}

//test("invariant 2 is inductive") {
//  val fs = List(
//    invariantProgress2,
//    tr,
//    Not(prime(invariantProgress2))
//  )
//  //assertUnsat(fs, 30000, false, cl3_2)
//}

//test("integrity") {
//  val fs = List(
//    invariantAgreement,
//    prime(invariantAgreement),
//    tr,
//    Not(integrity)
//  )
//  assertUnsat(fs, 30000, true, cl3_2)
//}

  test("validity is inductive") {
    val fs = List(
      //ForAll(List(i), Eq(Size(mailbox(i)),Literal(0))), //works with that (else branch)
      validity,
      tr,
      Not(prime(validity))
    )
    assertUnsat(fs, 10000, false, cl__3) //TODO check why this can go through (problem with inst ?) ...
    //assertUnsat(fs, 10000, true, cl__3, Some("test_validity.smt2"))
    //getModel(fs, 30000, cl__3) 
  }

}
