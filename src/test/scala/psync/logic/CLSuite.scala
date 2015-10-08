package psync.logic

import psync.formula._
import psync.utils.smtlib._
import dzufferey.utils.Logger
import TestCommon._

import org.scalatest._

class CLSuite extends FunSuite {

  val p = Variable("p").setType(pid)
  val p1 = Variable("p1").setType(pid)
  val p2 = Variable("p2").setType(pid)

  val nOver2 = Divides(n, Literal(2))
  val nOver3 = Divides(n, Literal(3)) 
  val twonOver3 = Divides(Times(n, Literal(2)), Literal(3))

  val a = Variable("A").setType(FSet(pid))
  val b = Variable("B").setType(FSet(pid))
  val c = Variable("C").setType(FSet(pid)) 
  val k = Variable("K").setType(FSet(pid)) 

  val x = Variable("x").setType(Int) 

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

/* TODO explicit bound on n will be hard to catch unless we build a complete model
  test("i notIn HO(i) > 0 and n=1"){
    val fs = List(
      Eq(a, Comprehension(List(i), Not(In(p, ho(i))))),
      ForAll(List(i), Geq(Cardinality(ho(i)), Literal(1))),
      ForAll(List(i), Not(In(i,ho(i)))), 
      Geq(Cardinality(a),Literal(1)),
      Eq(n, Literal(1))
    )
    assertSat(fs)
  }
*/

/* TODO explicit bound on n will be hard to catch unless we build a complete model
  test("epr and card(a) = n and n>2"){
    val fs = List(
      //Eq(data(i), Literal(3)),
      //Eq(Cardinality(ho(i)),Literal(1)),
      ForAll(List(i), Eq(i,p)),
      Eq(n,Literal(2)), 
      Eq(Cardinality(a), n), 
      Eq(Cardinality(b), n)
    )
    assertUnsat(fs)
  }
*/
  
  test("sat 1"){
    val fs = List(
      Exists(List(i), Eq(data(i),Literal(2))), 
      Not(Eq(Cardinality(Comprehension(List(i), Eq(data(i), Literal(3)))), nOver2)),
      ForAll(List(i), Or(Leq(data(p1), data(i)), Eq(data(p1),Literal(3)))), 
      Not(Exists(List(i), Eq(data(i), Literal(1)))), 
      Or(ForAll(List(i), Eq(data(i), Literal(1))), Eq(data(p),Literal(3)))
    )
    assertSat(fs)
    assertSat(fs, clg(2, 2))
  }

  test("Size of comprehension bigger than two"){
    val fs = List(
      Eq(a, Comprehension(List(i), ForAll(List(j), Implies(In(j,ho(i)), Lt(data(i), data(j)))))),
      Geq(Cardinality(a), Literal(2)),
      ForAll(List(i), SubsetEq(ho(i),a)),
      ForAll(List(i), Implies(In(i,a), ForAll(List(j), Implies(In(j,a), In(i,ho(j)))))),
      ForAll(List(i, j), Implies(Eq(i,j), (Eq(data(i),data(j))))),
      ForAll(List(i, j), Eq(data(i), data(j))),
      ForAll(List(i), Implies(In(i,ho(p1)), Lt(data(p1), data(i)))),
      ForAll(List(i), Implies(In(i,ho(p2)), Lt(data(p2), data(i))))
    )
    assertUnsat(fs)
    assertUnsat(fs, clg(2, 2))
  }

  test("Comprehension introduces new nodes"){
    val fs = List(
      Eq(a, Comprehension(List(i), Gt(Cardinality(ho(p)), Literal(1)))),
      Geq(Cardinality(a), Literal(1)),
      Eq(n, Literal(1))
    )
    assertUnsat(fs)
  }

  test("BAPA 0") {
    val fs = List(
      Eq(Cardinality(a), n),
      Eq(Cardinality(b), n),
      Eq(c, Intersection(a,b)),
      Eq(Cardinality(c), Literal(0))
    )
    assertUnsat(fs)
  }

  //from https://github.com/psuter/bapa-z3/blob/master/src/main/scala/bapa/Main.scala
  test("BAPA 1") {
    val fs = List(
      Not(Eq(a,b)),
      SubsetEq(a,b),
      Lt(Cardinality(b), Cardinality(Union(a, b)))
    )
    assertUnsat(fs)
  }

  //TODO better way to handle singletons
  //from https://github.com/psuter/bapa-z3/blob/master/src/main/scala/bapa/Main.scala
  test("BAPA 2") {
    val fs = List(
      Eq(a, Comprehension(List(i), Eq(i,p1))),
      Eq(b, Comprehension(List(i), Eq(i,p2))),
      Eq(Cardinality(a), Literal(1)), //added because we don't have singletons
      Eq(Cardinality(b), Literal(1)), //added because we don't have singletons
      Not(Eq(a,b)),
      SubsetEq(b,c),
      Lt(Cardinality(c), Cardinality(a))
    )
    assertUnsat(fs)
  }

  test("universe cardinality ⇒ ∀ (1)") {
    val fs = List(
      Eq(Cardinality(Comprehension(List(i), Eq(data(i), Literal(1)))), n),
      ForAll(List(i), Eq(data(i), Literal(0)))
    )
    assertUnsat(fs)
    //this example requires Eager instantiaton
    //  assertUnsat(fs, clg(2, 10))
  }

  test("universe cardinality ⇒ ∀ (2)") {
    val fs = List(
      Eq(Cardinality(Comprehension(List(i), Eq(data(i), Literal(1)))), n),
      Eq(data(j), Literal(0))
    )
    assertUnsat(fs)
  }

  test("cardinality two comprehensions intersect"){
    val fs = List(
       Eq(a, Comprehension(List(i), Eq(data(i), Literal(1)))),
       Eq(b, Comprehension(List(i), Eq(data(i), Literal(0)))),
       Gt(Cardinality(a), nOver2),
       Gt(Cardinality(b), nOver2)
    )        
    assertUnsat(fs)
  }

  test("cardinality three comprehensions"){
    val fs = List(
      Eq(a, Comprehension(List(i), Eq(data(i), Literal(1)))),
      Eq(b, Comprehension(List(i), Eq(data(i), Literal(0)))),
      Eq(c, Comprehension(List(i), Eq(data(i), x))),
      Gt(Cardinality(a), nOver2),
      Lt(Cardinality(b), nOver2),
      Gt(Cardinality(b), nOver3),
      Gt(Cardinality(c), twonOver3)
    )        
    assertUnsat(fs)
  }

  test("process j and one comprehension"){
    val fs = List(
      Eq(a, Comprehension(List(i), Eq(data(i), Literal(1)))),
      Eq(data(j), Literal(2)),
      Eq(Cardinality(a),n)
    )        
    assertUnsat(fs)
  }

  test("HO test: universals and comprehension"){
    val fs = List(
      Eq(a, Comprehension(List(i), Gt(Cardinality(ho(i)), nOver2))),
      Eq(Cardinality(a),n),
      ForAll(List(i), Lt(Cardinality(ho(i)), Literal(1)))
    )      
    assertUnsat(fs)
    assertUnsat(fs, clg(2, 2))
  }

  test("In Kernel and not in its HO"){
    val fs = List(
      Eq(a, Comprehension(List(i), Not(In(i, ho(i))))),
      Eq(k, Comprehension(List(i), ForAll(List(j), In(i,ho(j))))),
      Gt(Cardinality(a), nOver2),
      Gt(Cardinality(k), nOver2)
    )
    assertUnsat(fs)
  }

  test("Instantiate univ on set intersection"){
    val fs = List(
      Eq(a, Comprehension(List(i), Gt(data(i), Literal(1)))),
      Eq(b, Comprehension(List(i), Lt(data(i), Literal(3)))),
      Gt(Cardinality(a),nOver2),
      Gt(Cardinality(b),nOver2),
      ForAll(List(i), Not(Eq(data(i), Literal(2))))
    )
    assertUnsat(fs)
    //this example requires Eager instantiaton
    //  assertUnsat(fs, clg(2, 10))
  } 

  test("n = 0") {
    val fs = List(
      Eq(n, Literal(0))
    )
    assertUnsat(fs)
  }

  test("options 0") {
    val fs = List(
      IsDefined(FNone().setType(FOption(Int)))
    )
    assertUnsat(fs)
  }

  test("options 1") {
    val x = Variable("x").setType(FOption(pid))
    val none = FNone().setType(FOption(pid))
    val some = FSome(p1)
    val fs = List(
      Or(Eq(x, some), Eq(x, none)),
      Implies(
        IsDefined(x),
        Eq(Get(x), p1)
      )
    )
    assertSat(fs)
  }

  test("options 2") {
    val x = Variable("x").setType(FOption(pid))
    val none = FNone().setType(FOption(pid))
    val some = FSome(p1)
    val fs = List(
      Neq(p1, p2),
      Eq(x, some),
      Implies(
        IsDefined(x),
        Eq(Get(x), p2)
      )
    )
    assertUnsat(fs)
  }
  
  test("ordered") {
    val t = UnInterpreted("T")
    val t1 = Variable("t1").setType(t)
    val t2 = Variable("t2").setType(t)
    val t3 = Variable("t3").setType(t)
    assertUnsat(List(Leq(t1, t2), Leq(t2, t1), Not(Eq(t1, t2))))
    assertUnsat(List(Leq(t1, t2), Leq(t2, t3), Not(Leq(t1, t3))))
    assertUnsat(List(Lt(t1, t2), Lt(t2, t1)))
    assertSat(List(Leq(t1, t2), Leq(t2, t1)))
    assertSat(List(Leq(t1, t2), Leq(t2, t3), Leq(t3, t1)))
    assertUnsat(List(Leq(t1, t2), Leq(t2, t3), Leq(t3, t1), Not(Eq(t1, t3))))
  }

  //TODO tuples
  //test("pairs 0") {
  //}

}
