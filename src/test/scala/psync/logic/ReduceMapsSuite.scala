package psync.logic

import psync.formula._
import psync.utils.smtlib._
import dzufferey.utils.Logger
import TestCommon._

import org.scalatest._

class ReduceMapsSuite extends FunSuite {

  //TODO How to specify maps by Comp
  
  val m = Variable("m").setType(FMap(pid, Int))

  test("isDefinedAt 1") {
    val fs = List(
      Eq(Size(m), IntLit(0)),
      IsDefinedAt(m, i)
    )
    assertUnsat(fs)
  }

  test("isDefinedAt 2") {
    val fs = List(
      Geq(Size(m), IntLit(1)),
      IsDefinedAt(m, i)
    )
    assertSat(fs)
  }

  test("majority 1 unsat") {
    val mailbox = UnInterpretedFct("mailbox", Some(pid ~> FMap(pid,Int)))
    val data = UnInterpretedFct("data",Some(pid ~> Int))
    val half = Divides(n, IntLit(2))
    val fs = List(
      ForAll(List(i,j), And( Eq(IsDefinedAt(mailbox(i), j), In(j,ho(i))),
                             Eq(LookUp(mailbox(i), j), data(j)))),
      Gt(Cardinality(Comprehension(List(i), Eq(data(i), IntLit(0)))), half),
      Exists(List(i), And(
        Gt(Size(mailbox(i)), half),
        ForAll(List(j), Implies(IsDefinedAt(mailbox(i), j), Neq(LookUp(mailbox(i), j), IntLit(0))))
      ))
    )
    assertUnsat(fs)
  }

  test("majority 1 sat") { //compared to unsat: > replaced by ≥
    val mailbox = UnInterpretedFct("mailbox", Some(pid ~> FMap(pid,Int)))
    val data = UnInterpretedFct("data",Some(pid ~> Int))
    val half = Divides(n, IntLit(2))
    val fs = List(
      ForAll(List(i,j), And( Eq(IsDefinedAt(mailbox(i), j), In(j,ho(i))),
                             Eq(LookUp(mailbox(i), j), data(j)))),
      Geq(Cardinality(Comprehension(List(i), Eq(data(i), IntLit(0)))), half),
      Exists(List(i), And(
        Geq(Size(mailbox(i)), half),
        ForAll(List(j), Implies(IsDefinedAt(mailbox(i), j), Neq(LookUp(mailbox(i), j), IntLit(0))))
      ))
    )
    assertSat(fs)
  }

  test("majority 2 unsat") { //compared to 1: changed IsDefinedAt by KeySet
    val mailbox = UnInterpretedFct("mailbox", Some(pid ~> FMap(pid,Int)))
    val data = UnInterpretedFct("data",Some(pid ~> Int))
    val half = Divides(n, IntLit(2))
    val fs = List(
      ForAll(List(i,j), And( Eq(KeySet(mailbox(i)), ho(i)),
                             Eq(LookUp(mailbox(i), j), data(j)))),
      Gt(Cardinality(Comprehension(List(i), Eq(data(i), IntLit(0)))), half),
      Exists(List(i), And(
        Gt(Size(mailbox(i)), half),
        ForAll(List(j), Implies(IsDefinedAt(mailbox(i), j), Neq(LookUp(mailbox(i), j), IntLit(0))))
      ))
    )
    assertUnsat(fs)
  }

}
