package psync.logic

import psync.formula._
import psync.formula.InlineOps._
import TestCommon._

import org.scalatest._

class TpcExample extends FunSuite {
  
  val coord = Variable("coord").setType(pid)

  val v = Variable("v").setType(Bool)
  val s = Variable("s").setType(Int)

  val mailbox = UnInterpretedFct("mailbox", Some(pid ~> FMap(pid, Bool)))

  val data0 = UnInterpretedFct("data0",Some(pid ~> Bool))

  val data  = UnInterpretedFct("data", Some(pid ~> Bool))
  val data1 = UnInterpretedFct("data1",Some(pid ~> Bool))

  val decided  = UnInterpretedFct("decided", Some(pid ~> Bool))
  val decided1 = UnInterpretedFct("decided1",Some(pid ~> Bool))

  val vote  = UnInterpretedFct("vote", Some(pid ~> Bool))
  val vote1 = UnInterpretedFct("vote1",Some(pid ~> Bool))

  val primeMap = Map[Symbol,Symbol](
    data -> data1,
    decided -> decided1,
    vote -> vote1
  )
  def prime(f: Formula) = FormulaUtils.mapSymbol( x => primeMap.getOrElse(x, x), f)
  
  val agreement = ForAll(List(i,j), Implies(decided(i) && decided(j), data(i) === data(j)))
  val validity = ForAll(List(i), Exists(List(j), Implies(decided(i) && data(i), data0(j))))

  val initialState = ForAll(List(i), Not(vote(i)) && Not(decided(i)))

  /////////////
  // round 1 //
  /////////////

  val round1a =
    ForAll(List(i), And(
      Implies(
        And(i === coord,
            Comprehension(List(j), (j ∈ ho(i)) && data0(j)).card === n),
        vote1(i)
      ),
      Implies(
        And(i === coord,
            Not(Comprehension(List(j), (j ∈ ho(i)) && data0(j)).card === n)),
        Not(vote1(i))
      ),
      Implies(i !== coord, vote(i) === vote1(i)),
      data(i) === data1(i),
      decided(i) === decided1(i)
    ))

  val round1b = And(
    //send, mailbox
    ForAll(List(i, j), And(
        IsDefinedAt(mailbox(i),j) === (Eq(i, coord) && In(j, ho(i))),
        LookUp(mailbox(i), j) === data0(j)
      )
    ),
    //update
    ForAll(List(i), And(
      //coord
      Implies(i === coord,
        Exists(List(s), And(
          s === Comprehension(List(j), IsDefinedAt(mailbox(i), j) && LookUp(mailbox(i), j) === True()).card,
          Implies(s === n, vote1(i)),
          Implies(s !== n, Not(vote1(i)))
      ))),
      //non coord
      Implies(i !== coord, vote(i) === vote1(i)),
      //frame
      data(i) === data1(i),
      decided(i) === decided1(i)
    ))
  )

  /////////////
  // round 2 //
  /////////////

  val round2a =
    ForAll(List(i), And(
      Implies(
        In(coord,ho(i)),
        And(decided1(i), Eq(data1(i), vote(coord)))
      ),
      Implies(
        Not(In(coord,ho(i))),
        And(Not(decided1(i)), Eq(data1(i),data(i)))
      ),
      Eq(vote(i),vote1(i))
    ))

  val round2b = And(
    //send, mailbox
    ForAll(List(i,j), And(
        IsDefinedAt(mailbox(i), j) === (Eq(j, coord) && In(j, ho(i))),
        LookUp(mailbox(i), j) === vote(j)
      )
    ),
    //update
    ForAll(List(i), And(
      Or(
        IsDefinedAt(mailbox(i), coord) && decided1(i) && Eq(data1(i), LookUp(mailbox(i), coord)),
        Not(IsDefinedAt(mailbox(i), coord)) && Not(decided1(i)) && Eq(data1(i), data(i))
      ),
      Eq(vote(i),vote1(i))
    ))
  )

  val invariant1 = ForAll(List(i), And(
    Implies(vote(coord), data0(i)),
    Implies(decided(i), Eq(data(i), vote(coord)))
  ))

  test("invariant implies agreement") {
    val fs = List(invariant1, Not(agreement))
    assertUnsat(fs, cln(2, new quantifiers.Eager,  1, true))
    assertUnsat(fs, cln(2, new quantifiers.Guided, 1, true))
  }

  test("invariant implies validity") {
    val fs = List(invariant1, Not(validity))
    assertUnsat(fs, cln(2, new quantifiers.Eager,  1, true))
    assertUnsat(fs, cln(2, new quantifiers.Guided, 1, true))
  }

  test("initialState and round 1a implies invariant 1") {
    val fs = List(
      initialState,
      round1a,
      Not(prime(invariant1))
    )
    assertUnsat(fs, cln(1, new quantifiers.Eager,  1, true))
    assertUnsat(fs, cln(1, new quantifiers.Guided, 1, true))
    assertUnsat(fs, cln(2, new quantifiers.Eager,  1, true))
    assertUnsat(fs, cln(2, new quantifiers.Guided, 1, true))
  }

  test("invariant 1 is inductive at round 2a") {
    val fs = List(
      invariant1,
      round2a,
      Not(prime(invariant1))
    )
    assertUnsat(fs, cln(1, new quantifiers.Eager,  1, true))
    assertUnsat(fs, cln(1, new quantifiers.Guided, 1, true))
    assertUnsat(fs, cln(2, new quantifiers.Eager,  1, true))
    assertUnsat(fs, cln(2, new quantifiers.Guided, 1, true))
  }


  test("initialState and round 1b implies invariant 1") {
    val fs = List(
      initialState,
      round1b,
      Not(prime(invariant1))
    )
    assertUnsat(fs, cln(1, new quantifiers.Eager,  1, true))
    assertUnsat(fs, cln(1, new quantifiers.Guided, 1, true))
    assertUnsat(fs, cln(2, new quantifiers.Eager,  1, true))
    assertUnsat(fs, cln(2, new quantifiers.Guided, 1, true))
  }

  test("invariant 1 is inductive at round 2b") {
    val fs = List(
      invariant1,
      round2b,
      Not(prime(invariant1))
    )
    assertUnsat(fs, cln(1, new quantifiers.Eager,  1, true))
    assertUnsat(fs, cln(1, new quantifiers.Guided, 1, true))
    assertUnsat(fs, cln(2, new quantifiers.Eager,  1, true))
    assertUnsat(fs, cln(2, new quantifiers.Guided, 1, true))
  }

}
