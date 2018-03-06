package psync.logic

import psync.formula._
import psync.formula.InlineOps._
import TestCommon._

import org.scalatest._

//port of the example from the vmcai paper.
//they are more readable than dumping the VCs from the code
class LvExample extends FunSuite {

  val pld = UnInterpreted("payload")
  //a special type for the phase (try to reduce the blow-up)
  val phase = CL.timeType

  val r  = Variable("r").setType(phase)
  val r1 = Variable("r1").setType(phase)

  val a = Variable("A").setType(FSet(pid))

  val v = Variable("v").setType(pld) 
  val t = Variable("t").setType(phase) 

  val coord = UnInterpretedFct("coord", Some(pid ~> pid))

  val data0 = UnInterpretedFct("data0",Some(pid ~> pld))
  val data = UnInterpretedFct("data",Some(pid ~> pld))
  val data1 = UnInterpretedFct("data1",Some(pid ~> pld))

  val decided = UnInterpretedFct("decided", Some(pid ~> Bool))
  val decided1 = UnInterpretedFct("decided1", Some(pid ~> Bool))

  val vote = UnInterpretedFct("vote",Some(pid ~> pld))
  val vote1 = UnInterpretedFct("vote1",Some(pid ~> pld))

  val commit = UnInterpretedFct("commit",Some(pid ~> Bool))
  val commit1 = UnInterpretedFct("commit1",Some(pid ~> Bool))

  val ready = UnInterpretedFct("ready",Some(pid ~> Bool))
  val ready1 = UnInterpretedFct("ready1",Some(pid ~> Bool))
  
  val timeStamp = UnInterpretedFct("timeStamp",Some(pid ~> phase))
  val timeStamp1 = UnInterpretedFct("timeStamp1",Some(pid ~> phase))

  val primeMap = Map[Symbol,Symbol](
    data -> data1,
    decided -> decided1,
    vote -> vote1,
    commit -> commit1,
    ready -> ready1,
    timeStamp -> timeStamp1
  )
  def prime(f: Formula) = {
    val f1 = FormulaUtils.mapSymbol( x => primeMap.getOrElse(x, x), f)
    FormulaUtils.replace(r, r1, f1)
  }

  //properties
  val agreement = ForAll(List(i,j), Implies(And(decided(i), decided(j)), Eq(data(i),data(j))))
  val integrity = ForAll(List(i), Implies(decided(i), And(decided1(i), Eq(data(i), data1(i)))))
  val termination = ForAll(List(i), decided(i))
  val validity = ForAll(List(i), Exists(List(j), Eq(data(i), data0(j))))

  def majorityS(f: Formula) = Lt(n, Times(Literal(2), Cardinality(f)))
  def majorityM(f: Formula) = Lt(n, Times(Literal(2), Size(f)))

  val initialState = ForAll(List(i), And(
    Eq(decided(i), False()),
    Eq(ready(i), False()),
    Eq(commit(i), False()),
    Eq(data0(i), data(i))
  ))

  //transition relations

  val maxTS = UnInterpretedFct("maxTS", Some(FMap(pid,Product(pld,phase)) ~> pld))
  val maxTSdef = {
    val b = Variable("B").setType(FMap(pid,Product(pld,phase)))
    ForAll(List(b),
      Implies(
        Neq(Size(b), Literal(0)),
        Exists(List(j), And(
          IsDefinedAt(b, j),
          Eq(maxTS(b), Fst(LookUp(b, j))),
          ForAll(List(i),
            Implies(
              IsDefinedAt(b, i),
              Or( Eq(Fst(LookUp(b, i)), maxTS(b)),
                  Leq(Snd(LookUp(b, i)), Snd(LookUp(b, j)))
              )
            )
          )
        ))
      )
    )
  }

  val mailbox1 = UnInterpretedFct("mailbox", Some(pid ~> FMap(pid,Product(pld,phase))))
  val round1 = And(
    maxTSdef,
    //send, mailbox
    ForAll(List(i,j), And(
      Eq(IsDefinedAt(mailbox1(j), i), And(Eq(j, coord(i)), In(i, ho(j)))),
      Eq(LookUp(mailbox1(j), i), Tuple(data(i),timeStamp(i)))
    )),
    //update
    // then branch
    ForAll(List(i),
      Implies(And(Eq(i, coord(i)), majorityM(mailbox1(i))),
        And(Eq(vote1(i), maxTS(mailbox1(i))),
            commit1(i)))
    ),
    // else branch
    ForAll(List(i),
      Implies(Not(And(Eq(i, coord(i)), majorityM(mailbox1(i)))),
        Not(commit1(i)))
    ),
    // frame
    Eq(r, r1),
    ForAll(List(i), And(
      Eq(decided(i), decided1(i)),
      Eq(data(i), data1(i)),
      Eq(ready(i), ready1(i)),
      Eq(timeStamp(i), timeStamp1(i))
    ))
  )

  val mailbox2 = UnInterpretedFct("mailbox", Some(pid ~> FMap(pid,pld)))
  val round2 = And(
    //send, mailbox
    ForAll(List(i,j), And(
      Eq( IsDefinedAt(mailbox2(j), i),
          And(Eq(i, coord(i)), commit(i), In(i, ho(j))) ),
      Eq( LookUp(mailbox2(j), i), vote(i) )
    )),
    //update
    // then branch
    ForAll(List(i),
      Implies(IsDefinedAt(mailbox2(i), coord(i)),
        And(Eq(data1(i), LookUp(mailbox2(i), coord(i))), Eq(timeStamp1(i), r))
    )),
    // else branch
    ForAll(List(i),
      Implies(Not(IsDefinedAt(mailbox2(i), coord(i))),
        And(Eq(data1(i), data(i)), Eq(timeStamp1(i), timeStamp(i))))
    ),
    // frame
    Eq(r, r1),
    ForAll(List(i), And(
      Eq(decided(i), decided1(i)),
      Eq(ready(i), ready1(i)),
      Eq(commit(i), commit1(i)),
      Eq(vote(i), vote1(i))
    ))
  )

  val mailbox3 = UnInterpretedFct("mailbox", Some(pid ~> FSet(pid)))
  val round3 = And(
    //send, mailbox
    ForAll(List(i,j),
      Eq( In(i, mailbox3(j)),
          And(Eq(j, coord(i)),
              Eq(timeStamp(i), r),
              In(i, ho(j))))
    ),
    //update
    ForAll(List(i),
      Eq( And(Eq(i, coord(i)), majorityS(mailbox3(i))),
          ready1(i) )
    ),
    // frame
    Eq(r, r1),
    ForAll(List(i), And(
      Eq(decided(i), decided1(i)),
      Eq(data(i), data1(i)),
      Eq(commit(i), commit1(i)),
      Eq(vote(i), vote1(i)),
      Eq(timeStamp(i), timeStamp1(i))
    ))
  )

  val mailbox4 = UnInterpretedFct("mailbox", Some(pid ~> FMap(pid,pld)))
  val round4 = And(
    //send, mailbox
    ForAll(List(i), Eq(KeySet(mailbox4(i)), Comprehension(List(j), And(Eq(j, coord(j)), ready(j), In(j, ho(i)))))),
    ForAll(List(i,j), And(
      //Eq(IsDefinedAt(mailbox4(i), j), And(Eq(j, coord(j)), ready(j), In(j, ho(i)))),
      Eq(LookUp(mailbox4(i), j), vote(j))
    )),
    //update
    ForAll(List(i),
      Implies(IsDefinedAt(mailbox4(i), coord(i)),
        And(Eq(data1(i), LookUp(mailbox4(i), coord(i))),
            Eq(decided1(i), True())))),

    ForAll(List(i),
      Implies(Not(IsDefinedAt(mailbox4(i), coord(i))),
        And(Eq(data1(i), data(i)),
            Eq(decided1(i), decided(i))))),
    //Eq(Plus(r, Literal(1)), r1),
    Lt(r, r1), //replacing round by a phase type
    ForAll(List(i), And(
      //global update
      Eq(commit1(i), False()),
      Eq(ready1(i), False()),
      //frame
      Eq(decided(i), decided1(i)),
      Eq(data(i), data1(i)),
      Eq(vote(i), vote1(i)),
      Eq(timeStamp(i), timeStamp1(i))
    ))
  )

  //liveness assumption

  //TODO

  //invariants

  val invariant1 = And(
    Or(
      ForAll(List(i), And(Not(decided(i)), Not(ready(i)))),
      Exists(List(v,t,a), And(
        Eq(a, Comprehension(List(i), Leq(t, timeStamp(i)))),
        majorityS(a),
        Leq(t, r),
        ForAll(List(i), And(Implies(In(i, a), Eq(data(i), v)),
                            Implies(decided(i), Eq(data(i), v)),
                            Implies(commit(i), Eq(vote(i), v)),
                            Implies(ready(i), Eq(vote(i), v)),
                            Implies(Eq(timeStamp(i), r), commit(coord(i)))
                        )
        )
      ))
    ),
    ForAll(List(i), Exists(List(j), Eq(data(i), data0(j))))
  )
  
  //test VCs

  test("initial state implies invariant") {
    val fs = List(initialState, Not(invariant1))
    assertUnsat(fs, cln(2, new quantifiers.Eager(Some(1)), true))
  }

  test("invariant implies agreement") {
    val fs = List(invariant1, Not(agreement))
    assertUnsat(fs, cln(2, new quantifiers.Eager(Some(1)), true))
    assertUnsat(fs, cln(2, new quantifiers.Guided(Some(1)), true))
  }
  
  test("validity holds initially") {
    val fs = List(initialState, Not(validity))
    assertUnsat(fs, cln(2, new quantifiers.Eager(Some(1)), true))
    assertUnsat(fs, cln(2, new quantifiers.Guided(Some(1)), true))
  }

  test("maxTS") {
    val fs = List(
      maxTSdef,
      Eq(a, Comprehension(List(i), Leq(t, timeStamp(i)))),
      majorityS(a),
      ForAll(List(i), And(Implies(In(i, a), Eq(data(i), v)))),
      //ForAll(List(i), Eq(KeySet(mailbox1(i)), ho(i))),
      ForAll(List(i,j), And(
        Eq(IsDefinedAt(mailbox1(j), i), In(i, ho(j))),
        Eq(LookUp(mailbox1(j), i), Tuple(data(i),timeStamp(i)))
      )),
      majorityM(mailbox1(i)),
      Neq(maxTS(mailbox1(i)), v)
    )
    //getModel(fs)
    //assertUnsat(fs, cln(2, new quantifiers.Eager, 2, true)) //XXX this should be strictly more terms than Guided
    assertUnsat(fs, /*10000, true,*/ cln(2, new quantifiers.Guided(Some(2)), true))
  }

  //TODO those completely blow-up
  //for round 4, which should be "easy", the instantiation blows-up at the local step
  
//test("invariant 1 is inductive at round 1") {
//  val fs = List(
//    invariant1,
//    round1,
//    Not(prime(invariant1))
//  )
//  assertUnsat(fs, 60000, true, clg(2,3))
//  //getModel(fs, 60000, clg(2,3))
//  //assertUnsat(fs, 60000, true, clh(2,1,1))
//  //assertUnsat(fs, 60000, true, cl2_2, Some("test.smt2"))
//}

//test("invariant 1 is inductive at round 2") {
//  val fs = List(
//    invariant1,
//    round2,
//    Not(prime(invariant1))
//  )
//  //getModel(fs, 60000, clg(2,3))
//  assertUnsat(fs, 60000, true, clh(2,1,1))
//  assertUnsat(fs)
//}

//test("invariant 1 is inductive at round 3") {
//  val fs = List(
//    invariant1,
//    round3,
//    Not(prime(invariant1))
//  )
//  assertUnsat(fs)
//}

//test("invariant 1 is inductive at round 4") {
//  val fs = List(
//    invariant1,
//    round4,
//    Not(prime(invariant1))
//  )
//  //assertUnsat(fs, 30000, true, cln(2, new quantifiers.Eager, 2, true))
//  assertUnsat(fs, 30000, true, cln(2, new quantifiers.Sequence(new quantifiers.Eager, new quantifiers.Guided), 2, true))
//  //assertUnsat(fs, 30000, true, cln(0, new quantifiers.Guided, 2, false))
//}

}
