package psync.logic

import psync.formula._
import psync.formula.InlineOps._
import dzufferey.utils.Logger
import TestCommon._

import org.scalatest._




//port of the example from the vmcai paper.
//they are more readable than dumping the VCs from the code
class ZabDiscExampleNoMailbox extends FunSuite {


  val nOver2 = Divides(n, Literal(2))

  val pld = UnInterpreted("payload")
  //a special type for the phase (try to reduce the blow-up)
  val phase = CL.timeType

  val r  = Variable("r").setType(phase)
  val r1 = Variable("r1").setType(phase)

  val a = Variable("A").setType(FSet(pid))
  val b = Variable("B").setType(FSet(pid))

  val s = Variable("S").setType(FSet(pid))

  val leader = Variable("leader").setType(pid)
  val p = Variable("p").setType(pid)

  val v = Variable("v").setType(pld)
  val t = Variable("t").setType(phase)

  val coord = UnInterpretedFct("coord", Some(pid ~> pid))


  val decided = UnInterpretedFct("decided", Some(pid ~> Bool))
  val decided1 = UnInterpretedFct("decided1", Some(pid ~> Bool))


  val commit = UnInterpretedFct("commit",Some(pid ~> Bool))
  val commit1 = UnInterpretedFct("commit1",Some(pid ~> Bool))

  val ready = UnInterpretedFct("ready",Some(pid ~> Bool))
  val ready1 = UnInterpretedFct("ready1",Some(pid ~> Bool))

  val epoch0 = UnInterpretedFct("epoch",Some(pid ~> Int))
  val epoch = UnInterpretedFct("epoch",Some(pid ~> Int))
  val epoch1 = UnInterpretedFct("epoch1",Some(pid ~> Int))

  val jump = UnInterpretedFct("jump",Some(pid ~> Bool))
  val jump1 = UnInterpretedFct("jump1",Some(pid ~> Bool))

  val fjump = UnInterpretedFct("fjump",Some(pid ~> Bool))
  val fjump1 = UnInterpretedFct("fjump1",Some(pid ~> Bool))


  val primeMap = Map[Symbol,Symbol](
  //  data -> data1,
    decided -> decided1,
    commit -> commit1,
    ready -> ready1,
    epoch -> epoch1,
    jump -> jump1,
    fjump -> fjump1
  )
  def prime(f: Formula) = {
    val f1 = FormulaUtils.mapSymbol( x => primeMap.getOrElse(x, x), f)
    FormulaUtils.replace(r, r1, f1)
  }

  //properties
  val agreement = ForAll(List(i,j), Implies(And(decided(i), decided(j)), And(Eq(epoch(i),epoch(j)), Eq(coord(i),coord(j)))))
//  val integrity = ForAll(List(i), Implies(decided(i), And(decided1(i), Eq(data(i), data1(i)))))
  val termination = ForAll(List(i), decided(i))
//  val validity = ForAll(List(i), Exists(List(j), Eq(data(i), data0(j))))

  def majorityS(f: Formula) = Lt(n, Times(Literal(2), Cardinality(f)))
  def majorityM(f: Formula) = Lt(n, Times(Literal(2), Size(f)))

  val initialState = ForAll(List(i), And(
    Eq(fjump(i), False()),
    Eq(jump(i), False()),
    Eq(decided(i), False()),
    Eq(ready(i), False()),
    Eq(commit(i), False()),
    Eq(epoch0(i), epoch(i))
  ))

//   //transition relations
//
//   val maxEpoch = UnInterpretedFct("maxEpoch", Some(pid ~> pld))
//   val maxEpochdef = {
//     ForAll(List(i),
//       Implies(
//         Neq(ho(i).card, Literal(0)),
//         Exists(List(j), And(
//           j ∈ ho(i),
//           maxEpoch(i) === Plus(epoch(j),Literal(1)),
//           ForAll(List(k),
//             Implies(
//               k ∈ ho(i),
//                   epoch(k) <= epoch(j)
//             )
//           )
//         ))
//       )
//     )
//   }
//
//
val round1 = And(
  //update
  // then branch + mailbox
  ForAll(List(i,j),
    Implies(And (Eq(fjump(i), False()),
                 Eq(jump(i), False()),
                 Eq(i, coord(i)),
                 majorityS(ho(i))),
            And(Implies(In(j,ho(i)),
                         And(Implies(Neq(j,i),Leq(Plus(epoch(j),Literal(1)),epoch1(i))), Eq(coord(j),i))
                       ),
                Eq(ready1(i), True())
              )
        )
  ),
  //update
  // then branch + mailbox
  ForAll(List(i), Implies( Not(And (Eq(fjump(i), False()),
               Eq(jump(i), False()),
               Eq(i, coord(i)),
               majorityS(ho(i)))),
               //frame
               ForAll(List(i), And(
                 Eq(decided(i), False()),
                 Eq(epoch(i), epoch1(i)),
                 Eq(ready(i), False()),
                 Eq(commit(i), False()),
                 Eq(jump(i), jump1(i)),
                 Eq(fjump(i), fjump1(i))
               )))
  )
)

val round1a = And(
  //update
  // then branch + mailbox
  ForAll(List(i,j),
    Implies(And (Eq(i, coord(i)),
                 majorityS(ho(i)),
                 In(j,ho(i))),
            And(Eq(coord(j),i),
                Lt(epoch(j), epoch1(i)),
                Eq(ready1(i), True()),
                Eq(decided1(i), False())
              )
        )
  ),
  //update
  // else branch + mailbox
  ForAll(List(i,j),
    Implies(Not(And (Eq(i, coord(i)),
                 majorityS(ho(i)),
                 )),
                 And(Eq(ready1(i),False()),
                    Eq(decided1(i), False()),
                    Eq(epoch(i), epoch1(i))
                  )
               )
             )
)



  //liveness assumption

  //TODO

  //invariants



  val invariantV1a = ForAll(List(i), And(Eq(decided(i),False()), Eq(ready(i),False())))

  val invariantV1b =  Exists(List(leader), And(
       Eq(s,Comprehension(List(j), Eq(coord(j),leader))),
       majorityS(s),
       ForAll(List(i), And(
               Implies(And(In(i,s),Eq(ready(i),True())),
               And(Lt(epoch(i),epoch(leader)), Eq(coord(i),leader))),
              Implies(And(In(i,s),Eq(commit(i),True())), And(Eq(epoch(i),epoch(leader)), Eq(coord(i),leader))),
              Implies(Eq(decided(i),True()),And(Eq(epoch(i),epoch(leader)), Eq(coord(i),leader)))
        )
      )
    )
  )




 val invPart1 = ForAll(List(i), And(Eq(decided(i),False()), Eq(ready(i),False())))

 val invPart2 =  And( Eq(ready(leader), True()),
          ForAll(List(i), //And(
          Implies(In(i,s),
          And(Implies(Neq(i,leader),Lt(epoch(i ),epoch(leader))), Eq(coord(i),leader))))
        )
val invariantV1 =
          //  Or(
          //    invPart1,
               Exists(List(leader), And(
                   Eq(s,Comprehension(List(j),      And(Eq(coord(j),leader),Implies(Neq(j,leader),Lt(epoch(j),epoch(leader))))
                      )),
                   majorityS(s),
                   Eq(ready(leader),True())
                )
              )
          //  )


val invariantV1_noEpoch = //Or(
//invPart1,
 Exists(List(leader), And(
     Eq(s,Comprehension(List(j),Eq(coord(j),leader))),
     majorityS(s),
     Eq(ready(leader),True())
  ))
//)

 val NotInvariantV1 =
      And( Not(invPart1),
           ForAll(List(leader), And (
                  Eq(s,Comprehension(List(j),
                  //Eq(coord(j),leader))),
                  And(Eq(coord(j),leader),Implies(Neq(j,leader),Lt(epoch(j),epoch(leader)))))),
                  //Not(majorityS(s)),
                  Or(Not(majorityS(s)),Eq(ready(leader),False()))
                )))


val NotInvariantV1_noEpoch =
  And( Not(invPart1),
       ForAll(List(leader), And (
              Eq(s,Comprehension(List(j),
              Eq(coord(j),leader))),
              Or(Not(majorityS(s)),Eq(ready(leader),False()))
            )))


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
val test = Or(And(invariantV1_noEpoch, round1a,
                  prime(NotInvariantV1_noEpoch)),
              And(invPart1,round1a,
                  prime(NotInvariantV1_noEpoch)))

  ignore("round one if update condition dis no epoch "){
    val fs = List(test, True())


      //assertUnsat(fs,600000, true, c2e1)
      assertUnsat(fs,ClProc)
      //assertSat(fs, c2e2)
      //getModel(fs)
  }


  ignore("round one if update condition no epoch "){
    val fs = List(invariantV1_noEpoch,
             //invPart1,
             round1a,
             prime(NotInvariantV1_noEpoch))

      assertUnsat(fs, c2e2)

      //assertSat(fs, c2e2)
      //getModel(fs)
  }

   ignore("round one if update condition "){
     val fs = List(invariantV1,
              //invPart1,
              round1a,
              prime(NotInvariantV1))

       assertUnsat(fs, c2e2)

       //assertSat(fs, c2e2)
       //getModel(fs)
   }

  ignore("initial state implies invariant") {
    val fs = List(initialState, Not(invariantV1))
    assertUnsat(fs, c2e1)
  }

  ignore("invariant implies agreement") {
    val fs = List(invariantV1, Not(agreement))
    assertUnsat(fs, c2e1)
    //getModel(fs)
  }



  ignore("invariant 1 is inductive at round 1") {
    val fs = List(
      invariantV1a,
      round1,
      Not(prime(invariantV1))
    )
    //assertUnsat(fs,conf())
    assertSat(fs,conf())
  //  assertSat(fs, 60000, true, c2e2)
    getModel(fs)
    //getModel(fs, 60000, c2e2)
  }

  ignore("cardinality two comprehensions intersect"){
    val fs = List(
       Eq(a, Comprehension(List(i), Eq(epoch(i), Literal(1)))),
       Eq(b, Comprehension(List(i), Eq(epoch(i), Literal(0)))),
       majorityS(a),
       majorityS(b)
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
  }

}


  //
  // ignore("validity holds initially") {
  //   val fs = List(initialState, Not(validity))
  //   assertUnsat(fs, c2e1)
  // }
  //
  // ignore("maxEpoch") {
  //   val fs = List(
  //     maxEpochdef,
  //     Eq(a, Comprehension(List(i), Leq(t, timeStamp(i)))),
  //     majorityS(a),
  //     ForAll(List(i), And(Implies(In(i, a), Eq(data(i), v)))),
  //     majorityS(ho(i)),
  //     Neq(maxEpoch(i), v)
  //   )
  //   assertUnsat(fs, c2e1)
  // }
  //
  // ignore("frame") {
  //   val fs = List(
  //     invariant1,
  //     frame,
  //     Not(prime(invariant1))
  //   )
  //   assertUnsat(fs, 600000, true, c2e2)
  // }
  //
  // ignore("frame a") {
  //   val fs = List(
  //     invariant1a,
  //     frame,
  //     Not(prime(invariant1a))
  //   )
  //   //assertUnsat(fs, c2e2)
  //   assertUnsat(fs, conf())
  // }
  //
  // ignore("frame b") {
  //   val fs = List(
  //     invariant1b,
  //     frame,
  //     Not(prime(invariant1b))
  //   )
  //   //assertUnsat(fs, c2e2)
  //   assertUnsat(fs, conf())
  // }
  //
  // ignore("frame c") {
  //   val fs = List(
  //     invariant1c,
  //     frame,
  //     Not(prime(invariant1c))
  //   )
  //   assertUnsat(fs, conf())
  // }
  //
  // ignore("frame d") {
  //   val fs = List(
  //     invariant1d,
  //     frame,
  //     Not(prime(invariant1d))
  //   )
  //   //assertUnsat(fs, 600000, true, conf(1))
  //   //getModel(fs, 600000, conf(1))
  // }
  //
  // //TODO those completely blow-up
  // //for round 4, which should be "easy", the instantiation blows-up at the local step
  //

  //
  // ignore("invariant 1 is inductive at round 2") {
  //   val fs = List(
  //     invariant1,
  //     round2,
  //     Not(prime(invariant1))
  //   )
  //   //getModel(fs, 60000, clg(2,3))
  //   assertUnsat(fs, 60000, true, c2e2)
  // }
  //
  // ignore("invariant 1 is inductive at round 3") {
  //   val fs = List(
  //     invariant1,
  //     round3,
  //     Not(prime(invariant1))
  //   )
  //   assertUnsat(fs, c2e2)
  // }

  // ignore("invariant 1 is inductive at round 4") {
  //   val fs = List(
  //     invariant1,
  //     round4,
  //     Not(prime(invariant1))
  //   )
  //   assertUnsat(fs, 600000, true, conf())
  //   //getModel(fs, 60000, conf, fname = Some("model.smt2"), useCvcMf = true)
  // }

//}

//
//
//
//
//
//   val round1nomailbox = And(
//     maxEpochdef,
//     //update
//     // then branch
//     ForAll(List(i),
//       Implies(And (Fjump(i) === False(),
//                 And(jump(i) === False(),
//                   And(Eq(i, coord(i)), majorityS(ho(i))))),
//         And(Eq(epoch1(i), maxEpoch(i)),
//             commit1(i)))
//     ),
//     // else branch
//     ForAll(List(i),
//       Implies(Not(And(Eq(i, coord(i)), majorityS(ho(i)))),
//         And(Not(commit1(i)), Leq(epoch(i), epoch1(i))))
//     ),
//     // frame
//     Eq(r, r1),
//     ForAll(List(i), And(
//       Eq(decided(i), decided1(i)),
//       Eq(jump(i), jump1(i)),
//       Eq(Fjump(i), Fjump1(i)),
//       Eq(ready(i), ready1(i))
//     ))
//   )
//
//   val round2 = And(
//     //update
//     // frist then branch
//     ForAll(List(i),
//       Implies( And(Fjump(i) === False(), jump(i)===False()), And(Or(Eq(Fjump1(i),True()),Eq(Fjump1(i),False())), Eq(ready1(i), False()))
//     )),
//     // then branch
//     ForAll(List(i),
//       Implies(And(jump(i) === False(), coord(i) ∈ ho(i)),
//       And( Eq(ready1(i), True()), And(Eq(epoch1(i), epoch(coord(i), Eq(Fjump1(i),False())))))
//     )),
//     // else branch
//     ForAll(List(i),
//       Implies(coord(i) ∉ ho(i),
//         And(Leq(epoch(i),epoch1(i), Eq(ready1(i), False()))))
//     ),
//     // frame
//     Eq(r, r1),
//     ForAll(List(i), And(
//       Eq(decided(i), decided1(i)),
//       Eq(commit(i), commit1(i)),
//       Eq(jump(i), jump1(i)),
//     ))
//   )

  // val round3 = And(
  //   //update then
  //   ForAll(List(i),
  //     Implies( And(majorityS(ho(i)), Eq(ready1(i),True())),
  //     And( And(Eq(decided1(i),epoch(i)),Eq(data1(i),leader(i))), Eq(epoch1(i), Plus(epoch(i),Literal(1))))
  //       )
  //   ),
  //   //update then
  //   ForAll(List(i),
  //     Implies( Not(And(majorityS(ho(i)), Eq(ready1(i),True()))),
  //     And(Eq(decided1(i),decided(i)), Geq(epoch1(i), epoch(i))
  //       )
  //     )
  //   ),
  //   // frame
  //   Lt(r, r1),
  //   ForAll(List(i), And(
  //     //global update
  //       Eq(ready1(i), False()),
  //       Eq(commit1(i), False()),
  //       //frame
  //     Eq(vote(i), vote1(i)),
  //     Eq(epoch(i), epoch1(i))
  //   ))
  // )


  // val frame =
  //   ForAll(List(i), And(
  //     Eq(decided(i), decided1(i)),
  //     Eq(ready(i), ready1(i)),
  //     Eq(epoch(i), epoch1(i)),
  //     Eq(fjump(i), fjump1(i)),
  //     Eq(jump(i), jump1(i)),
  //     Eq(commit(i), commit1(i))
  //   )
  // )

  // ignore("frame") {
  //   val fs = List(
  //     invariantV1,
  //     frame,
  //     Not(prime(invariantV1))
  //   )
  //   assertUnsat(fs, 600000, false, c2e2)
  // }
  // val invariant1 =
  //   Or(
  //     ForAll(List(i), And(Not(decided(i)), Not(ready(i)))),
  //     Exists(List(v,t,a), And(
  //       Eq(a, Comprehension(List(i), Leq(t, timeStamp(i)))),
  //       majorityS(a),
  //       Leq(t, r),
  //       ForAll(List(i), And(Implies(In(i, a), Eq(data(i), v)),
  //                           Implies(decided(i), Eq(data(i), v)),
  //                           Implies(commit(i), Eq(vote(i), v)),
  //                           Implies(ready(i), Eq(vote(i), v)),
  //                           Implies(Eq(timeStamp(i), r), commit(coord(i)))
  //                       )
  //       )
  //     ))
  //   )
  //
  // //simpler version to help debugging
  // val invariant1a = And(
  //     Exists(List(j,t,a), And(
  //       Eq(a, Comprehension(List(i), Leq(t, timeStamp(i)))),
  //       majorityS(a),
  //       Leq(t, r),
  //       In(j, a),
  //       ForAll(List(i), And(Implies(In(i, a), Eq(data(i), data(j))),
  //                           Implies(decided(i), Eq(data(i), data(j))),
  //                           Implies(commit(i), Eq(vote(i), data(j))),
  //                           Implies(ready(i), Eq(vote(i), data(j))),
  //                           Implies(Eq(timeStamp(i), r), commit(coord(i)))
  //                       )
  //       )
  //     ))
  // )
  //
  // //even simpler version to help debugging
  // val invariant1b = {
  //   val a = Comprehension(List(i), Leq(t, timeStamp(i)))
  //     Exists(List(j,t), And(
  //       majorityS(a),
  //       Leq(t, r),
  //       In(j, a),
  //       ForAll(List(i), And(Implies(In(i, a), Eq(data(i), data(j))),
  //                           Implies(decided(i), Eq(data(i), data(j))),
  //                           Implies(commit(i), Eq(vote(i), data(j))),
  //                           Implies(ready(i), Eq(vote(i), data(j))),
  //                           Implies(Eq(timeStamp(i), r), commit(coord(i)))
  //                       ))
  //       )
  //     )
  // }
  //
  // //simpler version to help debugging
  // val invariant1c = {
  //   val a = Comprehension(List(l), Leq(t, timeStamp(l)))
  //   And(
  //     Or(
  //       ForAll(List(i), And(Not(decided(i)), Not(ready(i)))),
  //       Exists(List(v,t), And(
  //         majorityS(a),
  //         Leq(t, r),
  //         ForAll(List(j), And(Implies(In(j, a), Eq(data(j), v)),
  //                             Implies(decided(j), Eq(data(j), v)),
  //                             Implies(commit(j), Eq(vote(j), v)),
  //                             Implies(ready(j), Eq(vote(j), v))
  //                             //removed coord stuff
  //                         )
  //         )
  //       ))
  //     ),
  //     ForAll(List(k), Exists(List(m), Eq(data(k), data0(m))))
  //   )
  // }
  //
  // //simpler version to help debugging
  // val invariant1d = {
  //   val a = Comprehension(List(l), Leq(t, timeStamp(l)))
  //   And(
  //     Or(
  //       ForAll(List(i), And(Not(decided(i)), Not(ready(i)))),
  //       Exists(List(v,t), And(
  //         majorityS(a),
  //         Leq(t, r),
  //         ForAll(List(j), And(Implies(In(j, a), Eq(data(j), v)),
  //                             Implies(decided(j), Eq(data(j), v)),
  //                             Implies(commit(j), Eq(vote(j), v)),
  //                             Implies(ready(j), Eq(vote(j), v)),
  //                             Implies(Eq(timeStamp(j), r), commit(coord(j)))
  //                         )
  //         )
  //       ))
  //     ),
  //     ForAll(List(k), Exists(List(m), Eq(data(k), data0(m))))
  //   )
  // }
