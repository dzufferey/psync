package psync.logic

import psync.formula._
import psync.formula.InlineOps._
import TestCommon._

import org.scalatest._

//port of the example from the vmcai paper.
//they are more readable than dumping the VCs from the code
class OtrExample extends FunSuite {
    
  val pld = UnInterpreted("payload")

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
    //mmor def 
    ForAll(List(i,pld1), And(
      valueIs(mmor(i)).card >= 1, //required for validity
      valueIs(pld1).card <= valueIs(mmor(i)).card,
      Implies( valueIs(pld1).card === valueIs(mmor(i)).card, Leq(mmor(i), pld1))
    ))
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
              Implies(twoThird(a),      Eq(decided1(i), True())),
              Implies(Not(twoThird(a)), Eq(decided1(i), decided(i)))
        )))
      ),
      Implies(Not(twoThirdMap(mailbox(i))),
        And(Eq(decided(i), decided1(i)), Eq(data1(i), data(i)))
      )
    ))
  )

  val magicRound = Exists(List(a), And(
    twoThird(a),
    ForAll(List(i), Eq(ho(i), a))
  ))
  
  test("initial state implies invariant") {
    val fs = List(initialState, Not(invariantAgreement))
    assertUnsat(fs, cle(2, 2))
    assertUnsat(fs, clg(2, 2))
  }

  test("invariant implies agreement") {
    val fs = List(invariantAgreement, Not(agreement))
    assertUnsat(fs, cle(2, 2))
    assertUnsat(fs, clg(2, 2))
  }
  
  test("invariant implies termination") {
    val fs = List(invariantProgress2, Not(termination))
    assertUnsat(fs, cle(2, 2))
    assertUnsat(fs, clg(2, 2))
  }

  test("validity holds initially") {
    val fs = List(ForAll(List(i), Eq(data0(i), data(i))), Not(validity))
    assertUnsat(fs, cle(2, 2))
    assertUnsat(fs, clg(2, 2))
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
//    assertUnsat(fs, cle(3,1)) //XXX used to work
    assertUnsat(fs, clh(3,1,1))
    //assertUnsat(fs, clg(3,2))
    //assertUnsat(fs, 10000, true, cle(3,1))
    //assertUnsat(fs, 10000, true, clg(3,2))
  }
  
//test("invariant is inductive") {
//  val fs = List(
//    invariantAgreement,
//    tr,
//    Not(prime(invariantAgreement))
//  )
//  assertUnsat(fs, 30000, true, clh(3,1,2))
//  //assertUnsat(fs, 10000, false, cle(3,2))
//}

//test("1st magic round") {
//  val fs = List(
//    invariantAgreement,
//    magicRound,
//    tr,
//    Not(prime(invariantProgress1))
//  )
//  assertUnsat(fs, 10000, true, clh(3,1,2), Some("test.smt2"))
//  //assertUnsat(fs, 60000, true, cle(3,2))
//}

//test("invariant 1 is inductive") {
//  val fs = List(
//    invariantProgress1,
//    tr,
//    Not(prime(invariantProgress1))
//  )
//  //assertUnsat(fs, 30000, true, cle(3,2))
//  assertUnsat(fs, 30000, true, clh(3,1,2))
//}

//test("2nd magic round") {
//  val fs = List(
//    invariantProgress1,
//    magicRound,
//    tr,
//    Not(prime(invariantProgress2))
//  )
//  assertUnsat(fs, 10000, true, clh(3,1,1))
//  //assertUnsat(fs, 10000, true, cle(3,2))
//}

  test("invariant 2 is inductive") {
    val fs = List(
      invariantProgress2,
      tr,
      Not(prime(invariantProgress2))
    )
    assertUnsat(fs, cle(1,2))
    assertUnsat(fs, clh(1,2,1))
    //assertUnsat(fs, clg(1,2))
    //assertUnsat(fs, 10000, true, cle(1,2))
    //assertUnsat(fs, 10000, true, clg(1,2))
  }

//XXX used to work
//test("integrity") {
//  val fs = List(
//    invariantAgreement,
//    prime(invariantAgreement),
//    tr,
//    Not(integrity)
//  )
//  assertUnsat(fs, cle(3,2))
//  assertUnsat(fs, clh(3,1,1))
//  //assertUnsat(fs, clg(10,2))
////assertUnsat(fs, 10000, true, cle(4, 2))
////assertUnsat(fs, 10000, true, clg(10, 5))
//}

  test("validity is inductive") {
    val fs = List(
      invariantAgreement,
      prime(invariantAgreement),
      validity,
      tr,
      Not(prime(validity))
    )
    assertUnsat(fs, cle(1,1))
    assertUnsat(fs, clh(1,1,1))
    //assertUnsat(fs, clg(10,3))
  }

}
