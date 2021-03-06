package psync.logic

import psync.formula._
import psync.formula.InlineOps._
import dzufferey.utils.Logger
import TestCommon._

import org.scalatest.funsuite._

class CLSuite extends AnyFunSuite {

  val p = Variable("p").setType(pid)
  val p1 = Variable("p1").setType(pid)
  val p2 = Variable("p2").setType(pid)
  val p3 = Variable("p3").setType(pid)
  val p4 = Variable("p4").setType(pid)
  val p5 = Variable("p5").setType(pid)
  val p6 = Variable("p6").setType(pid)

  val nOver2 = Divides(n, Literal(2))
  val nOver3 = Divides(n, Literal(3)) 
  val twonOver3 = Divides(Times(n, Literal(2)), Literal(3))

  val a = Variable("A").setType(FSet(pid))
  val b = Variable("B").setType(FSet(pid))
  val c = Variable("C").setType(FSet(pid)) 
  val k = Variable("K").setType(FSet(pid)) 

  val x = Variable("x").setType(Int) 
  val y = Variable("y").setType(Int) 

  val data = UnInterpretedFct("data",Some(pid ~> Int))
  val decision = UnInterpretedFct("decision", Some(pid ~> Int))

  val m = UnInterpretedFct("M",Some(pid ~> FSet(pid))) //sender mailbox, dual of HO

/* TODO not sure about what happens with ∃ inside comprehension
  test("HO test: kernel and data"){
    val fs = List(
      Eq(k, Comprehension(List(i), ForAll (List(j), In(i,ho(j))))),
      Gt(Cardinality(k),nOver2),
      //Implies(In(i,ho(p)), Leq(data(j), data(i))), 
      Eq(a, Comprehension(List(i), Eq(data(i),x))),
      Gt(Cardinality(a), nOver2), 
      ForAll(List(i), Leq(x, data(i))),
      Eq(b, Comprehension(List(p), 
          Exists(List(j), And(Eq(data(j), decision(p)),
                              In(j, ho(p)), 
                              ForAll(List(i), Implies(In(i, ho(p)), Geq(data(j), data(i)))))
          )
        )
      ), 
      //Eq(Cardinality(b),n),
      //Exists(List(j), Gt(decision(j),x)),
      Exists(List(j), And(In(j,b), Gt(decision(j),x)))
    )       
    assertUnsat(fs)
  }
*/

  //explicit bound on n is hard to catch unless we build a complete model but ok when there are enough constants
  test("i notIn HO(i) > 0 and n=1"){
    val fs = List(
      a === Comprehension(List(i), Not(p ∈ ho(i))),
      ForAll(List(i), ho(i).card ≥ 1),
      a.card ≥ 1,
      n === 1
    )
    assertUnsat(fs)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("sat 1"){
    val fs = List(
      Exists(List(i), data(i) === 2), 
      Comprehension(List(i), data(i) === 3).card !== nOver2,
      ForAll(List(i), Or(data(p1) ≤ data(i), data(p1) === 3)), 
      Not(Exists(List(i), data(i) === 1)), 
      Or(ForAll(List(i), data(i) === 1), data(p) === 3)
    )
    assertSat(fs)
    assertSat(fs, c2e2)
    //assertSat(fs, onlyAxioms = true, useCvcMf = true, debug = true, fname = Some("sat1.smt2"))
  }

  test("Size of comprehension bigger than two"){
    val fs = List(
      a === Comprehension(List(i), ForAll(List(j), Implies(j ∈ ho(i), data(i) < data(j)))),
      a.card ≥ 2,
      ForAll(List(i), ho(i) ⊆ a),
      ForAll(List(i), Implies(i ∈ a, ForAll(List(j), Implies(j ∈ a, i ∈ ho(j))))),
      ForAll(List(i, j), Implies(i === j, data(i) === data(j))),
      ForAll(List(i, j), data(i) === data(j)),
      ForAll(List(i), Implies(i ∈ ho(p1), data(p1) < data(i))),
      ForAll(List(i), Implies(i ∈ ho(p2), data(p2) < data(i)))
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("Comprehension introduces new nodes"){
    val fs = List(
      a === Comprehension(List(i), ho(p).card > 1),
      a.card ≥ 1,
      n === 1
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("BAPA 0") {
    val fs = List(
      a.card === n,
      b.card === n,
      c === (a ∩ b),
      c.card === 0
    )
    assertUnsat(fs)
    assertUnsat(fs, onlyAxioms = true)
  }

  //from https://github.com/psuter/bapa-z3/blob/master/src/main/scala/bapa/Main.scala
  test("BAPA 1") {
    val fs = List(
      a !== b,
      a ⊆ b,
      b.card < (a ∪ b).card
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  //from https://github.com/psuter/bapa-z3/blob/master/src/main/scala/bapa/Main.scala
  test("BAPA 2") {
  val singleton = UnInterpretedFct("singleton",Some(pid ~> FSet(pid)))
    val fs = List(
      ForAll(List(i), And(
        singleton(i) === Comprehension(List(j), i === j),
        singleton(i).card === 1
      )),
      a === singleton(p1),
      b === singleton(p2),
      a !== b,
      b ⊆ c,
      c.card < a.card
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("universe cardinality ⇒ ∀ (1)") {
    val fs = List(
      Comprehension(List(i), data(i) === 1).card === n,
      ForAll(List(i), data(i) === 0)
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("universe cardinality ⇒ ∀ (1) (EPR)") {
    val data = UnInterpretedFct("data",Some(pid ~> Int ~> Bool))
    val fs = List(
      ForAll(List(x,y,i), Implies(data(i,x) ∧ data(i,y), x === y)),
      Comprehension(List(i), data(i, 1)).card === n,
      ForAll(List(i), data(i, 0))
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("universe cardinality ⇒ ∀ (2)") {
    val fs = List(
      Comprehension(List(i), data(i) === 1).card === n,
      data(j) === 0
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("universe cardinality ⇒ ∀ (2) (EPR)") {
    val data = UnInterpretedFct("data",Some(pid ~> Int ~> Bool))
    val fs = List(
      ForAll(List(x,y,i), Implies(data(i,x) ∧ data(i,y), x === y)),
      Comprehension(List(i), data(i, 1)).card === n,
      data(j, 0)
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("cardinality two comprehensions intersect"){
    val fs = List(
      a === Comprehension(List(i), data(i) === 1),
      b === Comprehension(List(i), data(i) === 0),
      a.card > nOver2,
      b.card > nOver2
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }
  
  test("cardinality two comprehensions intersect (EPR)"){
    val data = UnInterpretedFct("data",Some(pid ~> Int ~> Bool))
    val fs = List(
      ForAll(List(x,y,i), Implies(data(i,x) ∧ data(i,y), x === y)),
      a === Comprehension(List(i), data(i, 1)),
      b === Comprehension(List(i), data(i, 0)),
      a.card > nOver2,
      b.card > nOver2
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("cardinality three comprehensions"){
    val fs = List(
      a === Comprehension(List(i), data(i) === 1),
      b === Comprehension(List(i), data(i) === 0),
      c === Comprehension(List(i), data(i) === x),
      a.card > nOver2,
      b.card < nOver2,
      b.card > nOver3,
      c.card > twonOver3
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }
  
  test("cardinality three comprehensions (EPR)"){
    val data = UnInterpretedFct("data",Some(pid ~> Int ~> Bool))
    val fs = List(
      ForAll(List(x,y,i), Implies(data(i,x) ∧ data(i,y), x === y)),
      a === Comprehension(List(i), data(i, 1)),
      b === Comprehension(List(i), data(i, 0)),
      c === Comprehension(List(i), data(i, x)),
      a.card > nOver2,
      b.card < nOver2,
      b.card > nOver3,
      c.card > twonOver3
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("process j and one comprehension"){
    val fs = List(
      a === Comprehension(List(i), data(i) === 1),
      data(j) === 2,
      a.card === n
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("process j and one comprehension (EPR)"){
    val data = UnInterpretedFct("data",Some(pid ~> Int ~> Bool))
    val fs = List(
      ForAll(List(x,y,i), Implies(data(i,x) ∧ data(i,y), x === y)),
      a === Comprehension(List(i), data(i, 1)),
      data(j, 2),
      a.card === n
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("HO test: universals and comprehension"){
    val fs = List(
      a === Comprehension(List(i), ho(i).card > nOver2),
      a.card === n,
      ForAll(List(i), ho(i).card < 1)
    )      
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("In Kernel and not in its HO"){
    val fs = List(
      a === Comprehension(List(i), i ∉ ho(i)),
      k === Comprehension(List(i), ForAll(List(j), i ∈ ho(j))),
      a.card > nOver2,
      k.card > nOver2
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    //assertUnsat(fs, onlyAxioms = true) //TODO TO
  }
  
  test("In Kernel and not in its HO (EPR)"){
    val ho = UnInterpretedFct("HO",Some(pid ~> pid ~> Bool))
    val fs = List(
      a === Comprehension(List(i), Not(ho(i,i))),
      k === Comprehension(List(i), ForAll(List(j), ho(j,i))),
      a.card > nOver2,
      k.card > nOver2
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("Instantiate univ on set intersection"){
    val fs = List(
      a === Comprehension(List(i), data(i) > 1),
      b === Comprehension(List(i), data(i) < 3),
      a.card > nOver2,
      b.card > nOver2,
      ForAll(List(i), data(i) !== 2)
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  } 

  test("n = 0") {
    val fs = List(
      n === 0
    )
    assertUnsat(fs)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("options 0") {
    val fs = List(
      IsDefined(FNone().setType(FOption(Int)))
    )
    assertUnsat(fs)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("options 1") {
    val x = Variable("x").setType(FOption(pid))
    val none = FNone().setType(FOption(pid))
    val some = FSome(p1)
    val fs = List(
      Or(x === some, x === none),
      Implies(
        IsDefined(x),
        Get(x) === p1
      )
    )
    assertSat(fs)
    assertSat(fs, c2e2)
    assertSat(fs, onlyAxioms = true)
  }

  test("options 2") {
    val x = Variable("x").setType(FOption(pid))
    val none = FNone().setType(FOption(pid))
    val some = FSome(p1)
    val fs = List(
      p1 !== p2,
      x === some,
      Implies(
        IsDefined(x),
        Get(x) === p2
      )
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
  }
  
  test("ordered") {
    val t = UnInterpreted("T")
    val t1 = Variable("t1").setType(t)
    val t2 = Variable("t2").setType(t)
    val t3 = Variable("t3").setType(t)
    val unsat = Seq(
      List(Leq(t1, t2), Leq(t2, t1), Not(Eq(t1, t2))),
      List(Leq(t1, t2), Leq(t2, t3), Not(Leq(t1, t3))),
      List(Lt(t1, t2), Lt(t2, t1)),
      List(Leq(t1, t2), Leq(t2, t3), Leq(t3, t1), Not(Eq(t1, t3)))
    )
    val sat = Seq(
      List(Leq(t1, t2), Leq(t2, t1)),
      List(Leq(t1, t2), Leq(t2, t3), Leq(t3, t1))
    )
    unsat.foreach( fs => {
      assertUnsat(fs) 
      assertUnsat(fs, c2e2)
      assertUnsat(fs, onlyAxioms = true)
    })
    sat.foreach( fs => {
      assertSat(fs) 
      assertSat(fs, c2e2)
      assertSat(fs, onlyAxioms = true)
    })
  }

  test("lv 2x inv simple") {
    val t = CL.timeType
    val ts = UnInterpretedFct("ts",Some(pid ~> t))
    val d1 = Variable("d1").setType(Int)
    val d2 = Variable("d2").setType(Int)
    val fs = List(
      a === Comprehension(List(i), ts(i) ≥ Variable("tA")),
      b === Comprehension(List(i), ts(i) ≥ Variable("tB")),
      ForAll(List(i), (i ∈ a) ==> (data(i) === d1)),
      ForAll(List(i), (i ∈ b) ==> (data(i) === d2)),
      a.card > nOver2,
      b.card > nOver2,
      d1 !== d2
    )
    assertUnsat(fs)
    assertUnsat(fs, c2e2)
    assertUnsat(fs, onlyAxioms = true)
  }

  test("majority is a quorum") {
    val majority = UnInterpretedFct("majority",Some(FSet(pid) ~> Bool))
    val quorum = UnInterpretedFct("quorum",Some(FSet(pid) ~> FSet(pid) ~> Bool))
    val fs = List(
      ForAll(List(a), majority(a) === (a.card > n/2)),
      ForAll(List(a,b), quorum(a,b) === ((a ∩ b).card > 0)),
      majority(a),
      majority(b),
      !quorum(a, b)
    )
    assertUnsat(fs, c2e1)
    assertUnsat(fs, onlyAxioms = true)
  }
  
  test("2/3 majority is a fast quorum") {
    val majority = UnInterpretedFct("majority",Some(FSet(pid) ~> Bool))
    val quorum = UnInterpretedFct("quorum",Some(FSet(pid) ~> FSet(pid) ~> FSet(pid) ~> Bool))
    val fs = List(
      ForAll(List(a), majority(a) === (a.card > n*2/3)),
      ForAll(List(a,b,c), quorum(a,b,c) === (((a ∩ b) ∩ c).card > 0)),
      majority(a),
      majority(b),
      majority(c),
      !quorum(a, b, c)
    )
    assertUnsat(fs, c3e1)
    //assertUnsat(fs, onlyAxioms = true)
  }

  test("pairs 0") {
    val tpl1 = Variable("tpl1").setType(Product(List(pid, pid)))
    val tpl2 = Variable("tpl2").setType(Product(List(pid, pid)))
    val fs = List(
      tpl1 === Tuple(i, j),
      tpl2 === Tuple(l, j),
      tpl2._2 !== i
    )
    assertSat(fs)
    assertSat(fs, onlyAxioms = true)
    assertUnsat((tpl1._1 !== i) :: fs)
    assertUnsat((tpl1._1 !== i) :: fs, onlyAxioms = true)
  }

  //from https://github.com/CVC4/CVC4/blob/master/test/regress/regress0/sets/card.smt2
  test("cvc4-card-1") {
    val fs = List(
      a.card >= 5,
      b.card >= 5,
      (a ∪ b).card <= 4
    )
    assertUnsat(fs)
    assertUnsat(fs, onlyAxioms = true)
  }

  //from https://github.com/CVC4/CVC4/blob/master/test/regress/regress0/sets/card-2.smt2
  test("cvc4-card-2") {
    val fs = List(
      a.card >= 5,
      b.card >= 5,
      c.card <= 6,
      c === (a ∪ b)
    )
    assertSat(fs)
    //assertSat(fs, onlyAxioms = true, useCvcMf = true)
  }

  //from https://github.com/CVC4/CVC4/blob/master/test/regress/regress0/sets/card-3.smt2
  test("cvc4-card-3") {
    val fs = List(
      (a ∪ b).card >= 8,
      (a ∪ c).card >= 8,
      (b ∪ c).card <= 5,
      a.card <= 5,
      (b ∩ c) === Comprehension(List(i), False())
    )
    assertUnsat(fs, cln(10, new quantifiers.Eager(Some(1)), false))
    //assertUnsat(fs, onlyAxioms = true, debug = true, fname = Some("cvc-card-3.smt2"))
  }

  //from https://github.com/CVC4/CVC4/blob/master/test/regress/regress0/sets/card-4.smt2
  test("cvc4-card-4") {
    val fs = List(
      (a ∪ b).card >= 8,
      (a ∪ c).card >= 8,
      //(b ∪ c).card <= 5,
      a.card <= 5,
      (b ∩ c) === Comprehension(List(i), False()),
      p1 ∈ a,
      p2 ∈ a,
      p3 ∈ a,
      p4 ∈ a,
      p5 ∈ a,
      p6 ∈ a
    )
    assertSat(fs)
    //assertSat(fs, onlyAxioms = true, useCvcMf = true)
  }

  //from https://github.com/CVC4/CVC4/blob/master/test/regress/regress0/sets/card-5.smt2
  //TODO distinct membership will be hard to catch unless we build a complete model
  ignore("cvc4-card-5") {
    val fs = List(
      (a ∪ b).card >= 8,
      (a ∪ c).card >= 8,
      //(b ∪ c).card <= 5,
      a.card <= 5,
      (b ∩ c) === Comprehension(List(i), False()),
      p1 ∈ a,
      p2 ∈ a,
      p3 ∈ a,
      p4 ∈ a,
      p5 ∈ a,
      p6 ∈ a,
      p1 !== p2, p1 !== p3, p1 !== p4, p1 !== p5, p1 !== p6,
      p2 !== p3, p2 !== p4, p2 !== p5, p2 !== p6,
      p3 !== p4, p3 !== p5, p3 !== p6,
      p4 !== p5, p4 !== p6,
      p5 !== p6
    )
    assertUnsat(fs)
    assertUnsat(fs, onlyAxioms = true)
  }

  //from https://github.com/CVC4/CVC4/blob/master/test/regress/regress0/sets/card-6.smt2
  test("cvc4-card-6") {
    val fs = List(
      (a ∩ b) === Comprehension(List(i), False()),
      c ⊆ (a ∪ b),
      c.card >= 5,
      a.card <= 2,
      b.card <= 2
    )
    assertUnsat(fs, cln(10, new quantifiers.Eager(Some(1)), false))
    //assertUnsat(fs, onlyAxioms = true, debug = true)
  }

  //from https://github.com/CVC4/CVC4/blob/master/test/regress/regress0/sets/card-7.smt2
  test("cvc4-card-7") {
    val as = (1 to 20).map( i => Variable("A"+i).setType(FSet(pid)) )
    val ac = as.map( a => a.card )
    val ac1 = ac.map( ac => ac === 1 )
    val fs = (Plus(ac:_*) === 20) :: ac1.toList
    assertSat(fs, cln(1, new quantifiers.Eager(Some(1)), false))
    //assertSat(fs, onlyAxioms = true, debug = true)
  }

  test("map simple updates") {
    val k = UnInterpreted("K")
    val v = UnInterpreted("V")
    val k1 = Variable("k1").setType(k)
    val k2 = Variable("k2").setType(k)
    val v1 = Variable("v1").setType(v)
    val v2 = Variable("v2").setType(v)
    val m1 = Variable("M1").setType(FMap(k, v))
    val m2 = Variable("M2").setType(FMap(k, v))

    val sats = List(
      m1.updated(k1, v1).lookUp(k1) === v1,
      m1.updated(k1, v1).lookUp(k1) === v2,
      m1.updated(k1, v1).lookUp(k2) !== v1
    )
    val unsats = List(
      m1.updated(k1, v1).lookUp(k1) !== v1,
      Not(k1 ∈ m1.updated(k1, v1).keySet),
      Not(SubsetEq(m1.keySet, m1.updated(k1, v1).keySet))
    )
    sats.foreach( f => assertSat(List(f)) )
    unsats.foreach( f => assertUnsat(List(f)) )
    //sats.foreach( f => assertSat(List(f), onlyAxioms = true, useCvcMf = true) )
    unsats.foreach( f => assertUnsat(List(f), onlyAxioms = true) )
  }

  test("sets not equal") {
    val t = UnInterpreted("T")
    val s1 = Variable("S1").setType(FSet(t))
    val s2 = Variable("S2").setType(FSet(t))
    assertUnsat(List( s1 === s2, Not(s1 === s2) ))
    assertUnsat(List( s1 === s2, Not(s1 ⊆ s2) ))
    assertSat(List( Not(s2 ⊆ s1), Not(s1 ⊆ s2) ))
    assertUnsat(List( s1 === s2, Not(s1 === s2) ), onlyAxioms = true)
    assertUnsat(List( s1 === s2, Not(s1 ⊆ s2) ), onlyAxioms = true)
    assertSat(List( Not(s2 ⊆ s1), Not(s1 ⊆ s2) ), onlyAxioms = true)
  }

  test("arrays as maps with int keys") {
    val v = UnInterpreted("V")
    val v1 = Variable("v1").setType(v)
    val m1 = Variable("M1").setType(FMap(Int, v))
    val m2 = Variable("M2").setType(FMap(Int, v))
    val common = List(
      // x = max keyset
      m1.isDefinedAt(x),
      ForAll(List(y), m1.isDefinedAt(y) ==> y ≤ x),
      // update x+1
      m2 === m1.updated(x+1, v1)
    )
    // agree on keys less than x
    val valid = ForAll(List(y), ((y ≤ x) ∧ m1.isDefinedAt(y)) ==> (m1.lookUp(y) === m2.lookUp(y)) )
    assertUnsat(common :+ Not(valid)) // take negation for unsat
    assertSat(common :+ valid) // sanity check
    assertUnsat(common :+ Not(valid), onlyAxioms = true)
    //assertSat(common :+ valid, onlyAxioms = true, useCvcMf = true)
  }

}
