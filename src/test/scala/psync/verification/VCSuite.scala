package psync.verification

import psync.formula._
import psync.formula.InlineOps._
import psync.logic.CL
import psync.logic.TestCommon._
import psync.logic.quantifiers.{Eager, Guided, Sequence}

import org.scalatest._

class VCSuite extends FunSuite {

  test("Tcp") {
    import psync.logic.TpcExample._

    val cl = new CL(cln(1, new Eager,  1, true))

    val vcs = Seq(
      new SingleVC("invariant implies agreement", invariant1, True(), agreement, Nil, cl),
      new SingleVC("invariant implies validity", invariant1, True(), validity, Nil, cl),
      new SingleVC("initialState and round 1a implies invariant 1", initialState, round1a, invariant1P, Nil, cl),
      new SingleVC("invariant 1 is inductive at round 2a", invariant1, round2a, invariant1P, Nil, cl),
      new SingleVC("initialState and round 1b implies invariant 1", initialState, round1b, invariant1P, Nil, cl),
      new SingleVC("invariant 1 is inductive at round 2b", invariant1, round2b, invariant1P, Nil, cl)
    )

    vcs.foreach( v => {
      assert(v.isValid)
      assert(v.decompose.isValid)
    })
  }

  test("Otr") {
    import psync.logic.OtrExample._

    val cl = new CL(cln(2, new Guided,  1, true))
    val cl2 = new CL( cln(1, new Sequence(new Eager, new Guided), 2, true))

    val vcs = Seq(
      new SingleVC("invariant implies agreement", invariantAgreement, True(), agreement, Nil, cl),
      new SingleVC("initial state implies invariant", initialState, True(), invariantAgreement, Nil, cl),
      new SingleVC("invariant2 implies termination", invariantProgress2, True(), termination),
      new SingleVC("validity holds initially", ForAll(List(i), Eq(data0(i), data(i))), True(), validity)//,
      //new SingleVC("invariant is inductive", invariantAgreement, tr, prime(invariantAgreement), Nil, cl2) XXX case 1 is sat ???
    )

    vcs.foreach( v => {
      //assert(v.isValid)
      assert(v.decompose.isValid)
    })
  }

}
