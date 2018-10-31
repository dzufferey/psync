package psync.logic

import psync.formula._
import psync.formula.InlineOps._
import TestCommon._

import org.scalatest._

//port of the example from the vmcai paper.
//they are more readable than dumping the VCs from the code
class LvExampleNoMailbox extends FunSuite {

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

  val maxTS = UnInterpretedFct("maxTS", Some(pid ~> pld))
  val maxTSdef = {
    ForAll(List(i),
      Implies(
        Neq(ho(i).card, Literal(0)),
        Exists(List(j), And(
          j ∈ ho(i),
          maxTS(i) === data(j),
          ForAll(List(k),
            Implies(
              k ∈ ho(i),
              Or( data(k) === maxTS(i),
                  timeStamp(k) <= timeStamp(j)
              )
            )
          )
        ))
      )
    )
  }

  val round1 = And(
    maxTSdef,
    //update
    // then branch
    ForAll(List(i),
      Implies(And(Eq(i, coord(i)), majorityS(ho(i))),
        And(Eq(vote1(i), maxTS(i)),
            commit1(i)))
    ),
    // else branch
    ForAll(List(i),
      Implies(Not(And(Eq(i, coord(i)), majorityS(ho(i)))),
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

  val round2 = And(
    //update
    // then branch
    ForAll(List(i),
      Implies(coord(i) ∈ ho(i),
        And(Eq(data1(i), vote(coord(i))), Eq(timeStamp1(i), r))
    )),
    // else branch
    ForAll(List(i),
      Implies(coord(i) ∉ ho(i),
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

  val round3 = And(
    //update
    ForAll(List(i),
      Eq( And(Eq(i, coord(i)), majorityS(ho(i))),
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

  val round4 = And(
    //update
    ForAll(List(i),
      Implies(coord(i) ∈ ho(i),
        And(Eq(data1(i), vote(coord(i))),
            decided1(i)))),

    ForAll(List(i),
      Implies(coord(i) ∉ ho(i),
        And(Eq(data1(i), data(i)),
            Eq(decided1(i), decided(i))))),
    //Eq(Plus(r, Literal(1)), r1),
    Lt(r, r1), //replacing round by a phase type
    ForAll(List(i), And(
      //global update
      Eq(commit1(i), False()),
      Eq(ready1(i), False()),
      //frame
      Eq(vote(i), vote1(i)),
      Eq(timeStamp(i), timeStamp1(i))
    ))
  )

  val frame = And(
    Eq(r, r1),
    ForAll(List(i), And(
      Eq(vote(i), vote1(i)),
      Eq(ready(i), ready1(i)),
      Eq(timeStamp(i), timeStamp1(i)),
      Eq(decided(i), decided1(i)),
      Eq(data(i), data1(i)),
      Eq(commit(i), commit1(i))
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

  //simpler version to help debugging
  val invariant1a = And(
      Exists(List(j,t,a), And(
        Eq(a, Comprehension(List(i), Leq(t, timeStamp(i)))),
        majorityS(a),
        Leq(t, r),
        In(j, a),
        ForAll(List(i), And(Implies(In(i, a), Eq(data(i), data(j))),
                            Implies(decided(i), Eq(data(i), data(j))),
                            Implies(commit(i), Eq(vote(i), data(j))),
                            Implies(ready(i), Eq(vote(i), data(j))),
                            Implies(Eq(timeStamp(i), r), commit(coord(i)))
                        )
        )
      ))
  )
  
  //even simpler version to help debugging
  val invariant1b = {
    val a = Comprehension(List(i), Leq(t, timeStamp(i)))
      Exists(List(j,t), And(
        majorityS(a),
        Leq(t, r),
        In(j, a),
        ForAll(List(i), And(Implies(In(i, a), Eq(data(i), data(j))),
                            Implies(decided(i), Eq(data(i), data(j))),
                            Implies(commit(i), Eq(vote(i), data(j))),
                            Implies(ready(i), Eq(vote(i), data(j))),
                            Implies(Eq(timeStamp(i), r), commit(coord(i)))
                        ))
        )
      )
  }
  
  //simpler version to help debugging
  val invariant1c = {
    val a = Comprehension(List(l), Leq(t, timeStamp(l)))
    And(
      Or(
        ForAll(List(i), And(Not(decided(i)), Not(ready(i)))),
        Exists(List(v,t), And(
          majorityS(a),
          Leq(t, r),
          ForAll(List(j), And(Implies(In(j, a), Eq(data(j), v)),
                              Implies(decided(j), Eq(data(j), v)),
                              Implies(commit(j), Eq(vote(j), v)),
                              Implies(ready(j), Eq(vote(j), v))
                              //removed coord stuff
                          )
          )
        ))
      ),
      ForAll(List(k), Exists(List(m), Eq(data(k), data0(m))))
    )
  }

  //simpler version to help debugging
  val invariant1d = {
    val a = Comprehension(List(l), Leq(t, timeStamp(l)))
    And(
      Or(
        ForAll(List(i), And(Not(decided(i)), Not(ready(i)))),
        Exists(List(v,t), And(
          majorityS(a),
          Leq(t, r),
          ForAll(List(j), And(Implies(In(j, a), Eq(data(j), v)),
                              Implies(decided(j), Eq(data(j), v)),
                              Implies(commit(j), Eq(vote(j), v)),
                              Implies(ready(j), Eq(vote(j), v)),
                              Implies(Eq(timeStamp(j), r), commit(coord(j)))
                          )
          )
        ))
      ),
      ForAll(List(k), Exists(List(m), Eq(data(k), data0(m))))
    )
  }


  //specific tactic
  def conf(e: Int = 1) = {
    import psync.logic.quantifiers._
    val local = true
    val vennBound = 2
    val tactic = new Sequence(
        new Eager(Map[Type,Int](pid -> 1, FSet(pid) -> 1, phase -> 1, pld -> 1).withDefaultValue(0)),
        new Eager(e)
      )
    ClConfig(Some(vennBound), None, QStrategy(tactic, local))
  }
  
  
  //test VCs

  test("initial state implies invariant") {
    val fs = List(initialState, Not(invariant1))
    assertUnsat(fs, c2e1)
  }

  test("invariant implies agreement") {
    val fs = List(invariant1, Not(agreement))
    assertUnsat(fs, c2e1)
  }
  
  test("validity holds initially") {
    val fs = List(initialState, Not(validity))
    assertUnsat(fs, c2e1)
  }

  test("maxTS") {
    val fs = List(
      maxTSdef,
      Eq(a, Comprehension(List(i), Leq(t, timeStamp(i)))),
      majorityS(a),
      ForAll(List(i), And(Implies(In(i, a), Eq(data(i), v)))),
      majorityS(ho(i)),
      Neq(maxTS(i), v)
    )
    assertUnsat(fs, c2e1)
  }

  ignore("frame") {
    val fs = List(
      invariant1,
      frame,
      Not(prime(invariant1))
    )
    assertUnsat(fs, 600000, true, c2e2)
  }
  
  test("frame a") {
    val fs = List(
      invariant1a,
      frame,
      Not(prime(invariant1a))
    )
    //assertUnsat(fs, c2e2)
    assertUnsat(fs, conf())
  }

  test("frame b") {
    val fs = List(
      invariant1b,
      frame,
      Not(prime(invariant1b))
    )
    //assertUnsat(fs, c2e2)
    assertUnsat(fs, conf())
  }
  
  test("frame c") {
    val fs = List(
      invariant1c,
      frame,
      Not(prime(invariant1c))
    )
    assertUnsat(fs, conf())
  }

  ignore("frame d") {
    val fs = List(
      invariant1d,
      frame,
      Not(prime(invariant1d))
    )
    //assertUnsat(fs, 600000, true, conf(1))
    getModel(fs, 600000, conf(1))
  }

  //TODO those completely blow-up
  //for round 4, which should be "easy", the instantiation blows-up at the local step
  
  ignore("invariant 1 is inductive at round 1") {
    val fs = List(
      invariant1,
      round1,
      Not(prime(invariant1))
    )
    assertUnsat(fs, 60000, true, c2e2)
    //getModel(fs, 60000, clg(2,3))
  }

  ignore("invariant 1 is inductive at round 2") {
    val fs = List(
      invariant1,
      round2,
      Not(prime(invariant1))
    )
    //getModel(fs, 60000, clg(2,3))
    assertUnsat(fs, 60000, true, c2e2)
  }

  ignore("invariant 1 is inductive at round 3") {
    val fs = List(
      invariant1,
      round3,
      Not(prime(invariant1))
    )
    assertUnsat(fs, c2e2)
  }

  ignore("invariant 1 is inductive at round 4") {
    val fs = List(
      invariant1,
      round4,
      Not(prime(invariant1))
    )
    assertUnsat(fs, 600000, true, conf())
    //getModel(fs, 60000, conf, fname = Some("model.smt2"), useCvcMf = true)
  }

}
