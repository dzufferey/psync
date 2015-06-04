package round.logic

import round.formula._
import TestCommon._

import org.scalatest._

class TpcExample extends FunSuite {
  
  val pid = CL.procType

  val i = Variable("i").setType(pid)
  val j = Variable("j").setType(pid)
  val coord = Variable("coord").setType(pid)

  val ho = CL.HO
  val n = CL.n

  val v = Variable("v").setType(Bool)

  val mailbox = UnInterpretedFct("mailbox", Some(pid ~> FSet(Product(List(Bool,pid)))))

  val data0 = UnInterpretedFct("data0",Some(pid ~> Bool))

  val data = UnInterpretedFct("data",Some(pid ~> Bool))
  val data1 = UnInterpretedFct("data1",Some(pid ~> Bool))

  val decided = UnInterpretedFct("decided", Some(pid ~> Bool))
  val decided1 = UnInterpretedFct("decided1", Some(pid ~> Bool))

  val vote = UnInterpretedFct("vote",Some(pid ~> Bool))
  val vote1 = UnInterpretedFct("vote1",Some(pid ~> Bool))

  val primeMap = Map[Symbol,Symbol](
    data -> data1,
    decided -> decided1,
    vote -> vote1
  )
  def prime(f: Formula) = FormulaUtils.mapSymbol( x => primeMap.getOrElse(x, x), f)
  
  val agreement = ForAll(List(i,j), Implies(And(decided(i), decided(j)), Eq(data(i),data(j))))
  val validity = Exists(List(i), ForAll(List(j), Implies(And(decided(i), data(i)), data0(j))))

  val initialState = ForAll(List(i), And(Not(vote(i)), Not(decided(i))))

  /////////////
  // round 1 //
  /////////////

  val round1a =
    ForAll(List(i), And(
      Implies(
        And(Eq(i,coord),
            Eq(Cardinality(Comprehension(List(j), And(In(j, ho(i)), data0(j)))), n)),
        vote1(i)
      ),
      Implies(
        And(Eq(i,coord),
            Not(Eq(Cardinality(Comprehension(List(j), And(In(j, ho(i)), data0(j)))), n))),
        Not(vote1(i))
      ),
      Implies(
        Not(Eq(i,coord)),
        Eq(vote(i),vote1(i))
      ),
      Eq(data(i),data1(i)),
      Eq(decided(i),decided1(i))
    ))

  val round1b = And(
    //send, mailbox
    ForAll(List(i,j),
      Eq( In(Tuple(data0(i), i), mailbox(j)),
          And(Eq(j, coord),
              In(i, ho(j))))
    ),
    //update
    ForAll(List(i), And(
      //coord
      Implies(Eq(i,coord), And(
        Implies(Eq(Cardinality(Comprehension(List(j), In(Tuple(True(),j), mailbox(i)))), n),
                vote1(i)),
        Implies(Not(Eq(Cardinality(Comprehension(List(j), In(Tuple(True(),j), mailbox(i)))), n)),
                Not(vote1(i)))
      )),
      //non coord
      Implies(Not(Eq(i, coord)),
        Eq(vote(i),vote1(i))
      ),
      //frame
      Eq(data(i),data1(i)),
      Eq(decided(i),decided1(i))
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
    ForAll(List(i,j),
      Eq( In(Tuple(vote(i), i), mailbox(j)),
          And(Eq(i, coord),
              In(i, ho(j))))
    ),
    //update
    ForAll(List(i), And(
      Or(
        Exists(List(v), And(In(Tuple(v,coord),mailbox(i)), decided1(i), Eq(data1(i), v))),
        ForAll(List(v), And(Not(In(Tuple(v,coord),mailbox(i))),Not(decided1(i)), Eq(data1(i),data(i))))
      ),
      Eq(vote(i),vote1(i))
    ))
  )

  val invariant1 = ForAll(List(i), And(
    Implies(vote(coord), data0(i)),
    Implies(decided(i), Eq(data(i), vote(coord)))
  ))

  test("invariant implies agreement") {
    assertUnsat(List(invariant1, Not(agreement)))
  }

  test("invariant implies validity") {
    assertUnsat(List(invariant1, Not(validity)))
  }

  test("initialState and round 1 implies invariant 1a") {
    val fs = List(
      initialState,
      round1a,
      Not(prime(invariant1))
    )
    assertUnsat(fs)
  }

  test("invariant 1 is inductive at round 2a") {
    val fs = List(
      invariant1,
      round2a,
      Not(prime(invariant1))
    )
    assertUnsat(fs)
  }


//TODO figure out what is missing for that mailbox encoding

//test("initialState and round 1 implies invariant 1b") {
//  val fs = List(
//    initialState,
//    round1b,
//    Not(prime(invariant1))
//  )
//  //getModel(fs)
//  assertUnsat(fs)
//}

//test("invariant 1 is inductive at round 2b") {
//  val fs = List(
//    invariant1,
//    round2b,
//    Not(prime(invariant1))
//  )
//  assertUnsat(fs)
//}

}
