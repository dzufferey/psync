package round.logic

import round.formula._
import round.utils.smtlib._
import dzufferey.utils.Logger
import TestCommon._

import org.scalatest._

//port of the example from the vmcai paper.
//they are more readable than dumping the VCs from the code
class OtrExample extends FunSuite {

  val pid = CL.procType

  val i = Variable("i").setType(pid)
  val j = Variable("j").setType(pid)

  val ho = CL.HO
  val n = CL.n

  val a = Variable("A").setType(FSet(pid))

  val v = Variable("v").setType(Int) 

  val mailbox = UnInterpretedFct("mailbox", Some(pid ~> FSet(Product(List(Int,pid)))))

  val data = UnInterpretedFct("data",Some(pid ~> Int))
  val data1 = UnInterpretedFct("data1",Some(pid ~> Int))
  val data0 = UnInterpretedFct("data0",Some(pid ~> Int))
  val decided = UnInterpretedFct("decided", Some(pid ~> Bool))
  val decided1 = UnInterpretedFct("decided1", Some(pid ~> Bool))

  val agreement = ForAll(List(i,j), Implies(And(decided(i), decided(j)), Eq(data(i),data(j))))
  val integrity = ForAll(List(i), Implies(decided(i), And(decided1(i), Eq(data(i), data1(i)))))
  val termination = ForAll(List(i), decided(i))
  val validity = ForAll(List(i), Exists(List(j), Eq(data(i), data0(j))))
  val validityPrimed = ForAll(List(i), Exists(List(j), Eq(data1(i), data0(j))))

  def twoThird(f: Formula): Formula = {
    Lt(Times(n, Literal(2)), Times(Cardinality(f), Literal(3)))
  }

  val invariantAgreement = Or(
    ForAll(List(i), Not(decided(i))),
    Exists(List(v,a), And(
      Eq(a, Comprehension(List(i), Eq(data(i), v))),
      twoThird(a),
      ForAll(List(i), Implies(decided(i), Eq(data(i), v)))
    ))
  )
  
  val invariantAgreementPrimed = Or(
    ForAll(List(i), Not(decided1(i))),
    Exists(List(v,a), And(
      Eq(a, Comprehension(List(i), Eq(data1(i), v))),
      twoThird(a),
      ForAll(List(i), Implies(decided1(i), Eq(data1(i), v)))
    ))
  )

  val invariantProgress1 = Exists(List(v,a), And(
    Eq(a, Comprehension(List(i), Eq(data(i), v))),
    Eq(Cardinality(a), n),
    ForAll(List(i), Implies(decided(i), Eq(data(i), v)))
  ))
  
  val invariantProgress1Primed = Exists(List(v,a), And(
    Eq(a, Comprehension(List(i), Eq(data1(i), v))),
    Eq(Cardinality(a), n),
    ForAll(List(i), Implies(decided1(i), Eq(data1(i), v)))
  ))
  
  val invariantProgress2 =
    Exists(List(v), ForAll(List(i), And(decided(i), Eq(data(i), v))))

  val invariantProgress2Primed =
    Exists(List(v), ForAll(List(i), And(decided1(i), Eq(data1(i), v))))
  
  val initialState = ForAll(List(i), Eq(decided(i), False()))

  //TODO something seems wrong with the TR, mailbox, or mmor

  //min most often received
  val mmor = UnInterpretedFct("mmor", Some(pid ~> Int))
  val mmorDef = ForAll(List(i,v), Or(
    Lt( Cardinality(Comprehension(List(j), In(Tuple(v,j), mailbox(i)))),
        Cardinality(Comprehension(List(j), In(Tuple(mmor(i),j), mailbox(i))))),
    Leq(mmor(i), v)
  ))

  //transition relation
  val tr = And(
    //aux fun
    mmorDef,
    //send, mailbox
    ForAll(List(i,j), Eq(In(i,ho(j)), In(Tuple(data(i),j), mailbox(i)))),
    ForAll(List(i), Eq(Cardinality(mailbox(i)), Cardinality(ho(i)))),
    //update
    ForAll(List(i), And(
      Implies(twoThird(mailbox(i)),
        And(Eq(data1(i), mmor(i)),
            Exists(List(a), And(
              Eq(a, Comprehension(List(j), In(Tuple(mmor(i),j), mailbox(i)))),
              Implies(twoThird(a), Eq(decided1(i), True())),
              Implies(Not(twoThird(a)), Eq(decided1(i), decided(i)))
        )))
      ),
      Implies(Not(twoThird(mailbox(i))),
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

  //TODO some simple test for the encoding of the mailbox
  
  /*
  test("invariant is inductive") {
    val fs = List(
      //ForAll(List(i), Eq(Cardinality(mailbox(i)),Literal(0))), //try the else branch
      invariantAgreement,
      tr,
      Not(invariantAgreementPrimed)
    )
    assertUnsat(fs)
  }
  */

  /*
  test("1st magic round") {
    val fs = List(
      invariantAgreement,
      magicRound
      tr,
      Not(invariantProgress1Primed)
    )
    assertUnsat(fs)
  }
  */
  
  /*
  test("invariant 1 is inductive") {
    val fs = List(
      invariantProgress1,
      tr,
      Not(invariantProgress1Primed)
    )
    assertUnsat(fs)
  }
  */
  
  /*
  test("2nd magic round") {
    val fs = List(
      invariantProgress1,
      magicRound
      tr,
      Not(invariantProgress2Primed)
    )
    assertUnsat(fs)
  }
  */

  /*
  test("invariant 2 is inductive") {
    val fs = List(
      invariantProgress2,
      tr,
      Not(invariantProgress2Primed)
    )
    assertUnsat(fs)
  }
  */
  
  /*
  test("integrity") {
    val fs = List(
      invariantAgreement,
      invariantAgreementPrimed,
      tr,
      Not(integrity)
    )
    assertUnsat(fs)
  }
  */

  /*
  test("validity is inductive") {
    val fs = List(
      validity,
      tr,
      Not(validityPrimed)
    )
    assertUnsat(fs)
  }
  */

}
