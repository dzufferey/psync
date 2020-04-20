package psync.logic

import psync.formula._
import psync.formula.InlineOps._
import TestCommon._

import org.scalatest.funsuite._

//port of the example from the vmcai paper.
//they are more readable than dumping the VCs from the code
class OtrExampleNoMailbox extends AnyFunSuite {

  val pld = UnInterpreted("payload")

  val a = Variable("A").setType(FSet(pid))

  val v = Variable("v").setType(pld) 

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

  def valueIs(f: Formula) = Comprehension(List(j), And( In(j, ho(i)), Eq(data(j), f)))

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
    //update
    ForAll(List(i), And(
      Implies(twoThird(ho(i)),
        And(Eq(data1(i), mmor(i)),
            Exists(List(a), And(
              Eq(a, valueIs(mmor(i))),
              Implies(twoThird(a),      Eq(decided1(i), True())),
              Implies(Not(twoThird(a)), Eq(decided1(i), decided(i)))
        )))
      ),
      Implies(Not(twoThird(ho(i))),
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
    assertUnsat(fs, c2e1)
  }

  test("invariant implies agreement") {
    val fs = List(invariantAgreement, Not(agreement))
    assertUnsat(fs, c2e1)
  }
  
  test("invariant implies termination") {
    val fs = List(invariantProgress2, Not(termination))
    assertUnsat(fs, c2e1)
  }

  test("validity holds initially") {
    val fs = List(ForAll(List(i), Eq(data0(i), data(i))), Not(validity))
    assertUnsat(fs, c2e1)
  }

  test("mmor unsat") {
    val fs = List(
      defs,
      //env assumptions
      Comprehension(List(i), data(i) === v).card > ((n * 2) / 3),
      ForAll(List(i), ho(i).card > ((n * 2) / 3) ),
      //negated prop: ¬(∀ k. mmor(k) == v)
      mmor(k) !== v
    )
    assertUnsat(fs, c3e1)
  }
  
  ignore("invariant is inductive") {
    val fs = List(
      invariantAgreement,
      tr,
      Not(prime(invariantAgreement))
    )
    assertUnsat(fs, to = 600000, reducer = c3e2)
  }

  ignore("1st magic round") {
    val fs = List(
      invariantAgreement,
      magicRound,
      tr,
      Not(prime(invariantProgress1))
    )
    assertUnsat(fs, to = 600000, reducer = c3e2)
  }

  ignore("invariant 1 is inductive") {
    val fs = List(
      invariantProgress1,
      tr,
      Not(prime(invariantProgress1))
    )
    assertUnsat(fs, to = 600000, reducer = c3e2, dnfExpansion = true)
  }

  test("2nd magic round") {
    val fs = List(
      invariantProgress1,
      magicRound,
      tr,
      Not(prime(invariantProgress2))
    )
    assertUnsat(fs, c3e2)
  }

  test("invariant 2 is inductive") {
    val fs = List(
      invariantProgress2,
      tr,
      Not(prime(invariantProgress2))
    )
    assertUnsat(fs, c1e2)
  }

  test("integrity") {
    val fs = List(
      invariantAgreement,
      prime(invariantAgreement),
      tr,
      Not(integrity)
    )
    assertUnsat(fs, to = 120000, reducer = c3e1)
  }

  test("validity is inductive") {
    val fs = List(
      invariantAgreement,
      prime(invariantAgreement),
      validity,
      tr,
      Not(prime(validity))
    )
    assertUnsat(fs, c1e1)
  }

}
