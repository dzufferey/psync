package round.logic

import round.formula._
import TestCommon._

import org.scalatest._

//port of the example from the vmcai paper.
//they are more readable than dumping the VCs from the code
class LvExample extends FunSuite {

  val pid = CL.procType
  val pld = UnInterpreted("payload")

  val i = Variable("i").setType(pid)
  val j = Variable("j").setType(pid)

  val ho = CL.HO
  val n = CL.n
  val r = Variable("r").setType(Int)
  val r1 = Variable("r1").setType(Int)

  val a = Variable("A").setType(FSet(pid))

  val v = Variable("v").setType(pld) 
  val t = Variable("t").setType(Int) 

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
  
  val timeStamp = UnInterpretedFct("timeStamp",Some(pid ~> Int))
  val timeStamp1 = UnInterpretedFct("timeStamp1",Some(pid ~> Int))

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

  def majority(f: Formula) = Lt(n, Times(Literal(2), Cardinality(f)))

  val initialState = ForAll(List(i), And(
    Eq(decided(i), False()),
    Eq(ready(i), False()),
    Eq(commit(i), False()),
    Eq(data0(i), data(i))
  ))

  //transition relations

  val mt = Variable("mt").setType(Int) 
  val maxTS = UnInterpretedFct("maxTS", Some(FSet(Product(Product(pld,Int),pid)) ~> pld))
  val b = Variable("B").setType(FSet(Product(Product(pld,Int),pid)))

  val mailbox1 = UnInterpretedFct("mailbox", Some(pid ~> FSet(Product(Product(pld,Int),pid))))
  val round1 = And(
    //aux fun
    ForAll(List(b), Exists(List(mt,j),
      Implies(
        Eq(Cardinality(b), Literal(0)),
        And(
          In(Tuple(Tuple(maxTS(b),mt),j), b),
          ForAll(List(i,v,t),
            Implies(
              In(Tuple(Tuple(v,t),i), b),
              Or(Eq(v, maxTS(b)), Lt(t, mt))
    ))) ) ) ),
    //send, mailbox
    ForAll(List(i,j),
      Eq( In(Tuple(Tuple(data(i),timeStamp(i)), i), mailbox1(j)),
          And(Eq(i, coord(i),
              commit(i),
              In(i, ho(j)))))
    ),
    //update
    // then branch
    ForAll(List(i),
      Implies(And(Eq(i, coord(i)), majority(mailbox1(i))),
        And(Eq(vote1(i), maxTS(mailbox1(i))),
            commit1(i)))
    ),
    // else branch
    ForAll(List(i),
      Implies(Not(And(Eq(i, coord(i)), majority(mailbox1(i)))),
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

  val mailbox2 = UnInterpretedFct("mailbox", Some(pid ~> FSet(Product(pld,pid))))
  val round2 = And(
    //send, mailbox
    ForAll(List(i,j),
      Eq( In(Tuple(vote(i), i), mailbox2(j)),
          And(Eq(i, coord(i)),
              commit(i),
              In(i, ho(j))))
    ),
    //update
    // then branch
    ForAll(List(i), Exists(List(v),
      Implies(In(Tuple(v,coord(i)), mailbox2(i)),
        And(Eq(data1(i), v), Eq(timeStamp1(i), r)))
    )),
    // else branch
    ForAll(List(i,v),
      Implies(Not(In(v,coord(i), mailbox2(i))),
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
      Eq( And(Eq(i, coord(i)), majority(mailbox3(i))),
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

//val mailbox4 = UnInterpretedFct("mailbox", Some(pid ~> FSet(Product(pld,pid))))
//val round4 = And(
//  //send, mailbox
//  ForAll(List(i,j),
//    Eq( In(Tuple(vote(i), i), mailbox4(j)),
//        And(Eq(i, coord(i)),
//            ready(i),
//            In(i, ho(j))))
//  ),
//  //update
//  ForAll(List(i), Exists(List(v),
//    Implies(In(Tuple(v,coord(i)), mailbox4(i)),
//      And(Eq(data1(i), v),
//          Eq(decided1(i), True())))
//  )),
//  ForAll(List(i,v),
//    Implies(Not(In(Tuple(v,coord(i)), mailbox4(i))),
//      And(Eq(data1(i), data(i)),
//          Eq(decided1(i), decided(i))))
//  ),
//  Eq(Plus(r, Literal(1)), r1),
//  ForAll(List(i), And(
//    //global update
//    Eq(commit1(i), False()),
//    Eq(ready1(i), False()),
//    //frame
//    Eq(decided(i), decided1(i)),
//    Eq(data(i), data1(i)),
//    Eq(vote(i), vote1(i)),
//    Eq(timeStamp(i), timeStamp1(i))
//  ))
//)
  val mailbox4 = UnInterpretedFct("mailbox", Some(pid ~> FSet(pid)))
  val round4 = And(
    //send, mailbox
    ForAll(List(j),
      Eq(mailbox4(j), Comprehension(List(i), And(Eq(i, coord(i)), ready(i), In(i, ho(j)))))
    ),
    //update
    ForAll(List(i),
      Implies(In(coord(i), mailbox4(i)),
        And(Eq(data1(i), vote(coord(i))),
            Eq(decided1(i), True())))
    ),
    ForAll(List(i),
      Implies(Not(In(coord(i), mailbox4(i))),
        And(Eq(data1(i), data(i)),
            Eq(decided1(i), decided(i))))
    ),
    Eq(Plus(r, Literal(1)), r1),
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

  val invariant1 = Or(
    ForAll(List(i), And(Not(decided(i)), Not(ready(i)))),
    Exists(List(v,t,a), And(
      Eq(a, Comprehension(List(i), Leq(t, timeStamp(i)))),
      majority(a),
      Leq(t, r),
      ForAll(List(i), And(Implies(In(i, a), Eq(data(i), v)),
                          Implies(decided(i), Eq(data(i), v)),
                          Implies(commit(i), Eq(vote(i), v)),
                          Implies(ready(i), Eq(vote(i), v)),
                          Implies(Eq(timeStamp(i), r), commit(coord(i)))
                      )
      )
      //TODO validity
    ))
  )

  //test VCs

  test("initial state implies invariant") {
    assertUnsat(List(initialState, Not(invariant1)))
  }
  
  test("invariant implies agreement") {
    assertUnsat(List(invariant1, Not(agreement)))
  }
  
  test("validity holds initially") {
    assertUnsat(List(initialState, Not(validity)))
  }

  //TODO those completely blow-up
  
//test("invariant 1 is inductive at round 1") {
//  val fs = List(
//    invariant1,
//    round1,
//    Not(prime(invariant1))
//  )
//  assertUnsat(fs)//, 60000, true, Some("test.smt2"))
//}

//test("invariant 1 is inductive at round 2") {
//  val fs = List(
//    invariant1,
//    round2,
//    Not(prime(invariant1))
//  )
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
//  //assertUnsat(fs)//, 60000, true, Some("test.smt2"))
//  getModel(fs, 60000)
//}


}
