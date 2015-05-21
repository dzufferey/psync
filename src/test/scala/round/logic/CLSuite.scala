package round.logic

import round.formula._
import round.utils.smtlib._
import dzufferey.utils.Logger

import org.scalatest._

class CLSuite extends FunSuite {

  val pid = CL.procType

  val i = Variable("i").setType(pid)
  val j = Variable("j").setType(pid)
  val p = Variable("p").setType(pid)
  val p1 = Variable("p1").setType(pid)
  val p2 = Variable("p2").setType(pid)

  val n = CL.n
  val nOver2 = Divides(n, Literal(2))
  val nOver3 = Divides(n, Literal(3)) 
  val twonOver3 = Divides(Times(n, Literal(2)), Literal(3))

  val a = Variable("A").setType(FSet(pid))
  val b = Variable("B").setType(FSet(pid))
  val c = Variable("C").setType(FSet(pid)) 
  val k = Variable("K").setType(FSet(pid)) 

  val x = Variable("x").setType(Int) 

  val _data = UnInterpretedFct("data",Some(pid ~> Int))
  def data(i: Formula) = Application(_data, List(i)).setType(Int)

  val _dec = UnInterpretedFct("dec", Some(pid ~> Int))
  def dec(i: Formula) = Application(_dec, List(i)).setType(Int)

  val m = UnInterpretedFct("M",Some(pid ~> FSet(pid))) //sender mailbox, dual of HO
  val ho = CL.HO

  def assertUnsat(conjuncts: List[Formula]) {
    val c0 = conjuncts.map(Simplify.simplify)
    val f0 = And(c0 :_*)
    val f1 = CL.reduce(f0)
    val solver = Solver(UFLIA)
    assert(!solver.testB(f1), "unsat formula")
  }
  
  def assertUnsatDebug(conjuncts: List[Formula]) {
    Logger.moreVerbose
    val c0 = conjuncts.map(Simplify.simplify)
    println("=======before reduce ")
    c0.foreach( f => println("  " + f) )
    val f0 = And(c0 :_*)
    val f1 = CL.reduce(f0)
    println("======= send to solver")
    FormulaUtils.getConjuncts(f1).foreach( f => println("  " + f) )
    //val solver = Solver(UFLIA, "test.smt2")
    //val solver = Solver.cvc4mf(UFLIA, None, 10000)
    val solver = Solver(UFLIA)
    Logger.lessVerbose
    assert(!solver.testB(f1), "unsat formula")
  }

  def assertSat(conjuncts: List[Formula]) {
    val c0 = conjuncts.map(Simplify.simplify)
    //Simplify is in src/main/scala/round/formula/simplify  
    val f0 = And(c0 :_*)
    // this call apply from formula in class symbol line 93
    //println("=======before reduce ")
    //c0.foreach( f => println("  " + f) )
    val f1 = CL.reduce(f0)
    //println("======= send to solver")
    //FormulaUtils.getConjuncts(f1).foreach( f => println("  " + f) )
    val solver = Solver(UFLIA)
    assert( solver.testB(f1), "sat formula")
  }

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
          Exists(List(j), And(Eq(data(j), dec(p)),
                              In(j, ho(p)), 
                              ForAll(List(i), Implies(In(i, ho(p)), Geq(data(j), data(i)))))
          )
        )
      ), 
      //Eq(Cardinality(b),n),
      //Exists(List(j), Gt(dec(j),x)),
      Exists(List(j), And(In(j,b), Gt(dec(j),x)))
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
  }

  test("Comprehention introduces new nodes"){
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

  test("universe cardinality ⇒ ∀") {
    val fs = List(
      Eq(a, Comprehension(List(i), Eq(data(i), Literal(1)))),
      Eq(Cardinality(a), n),
      ForAll(List(i), Eq(data(i), Literal(0)))
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

  //TODO tuples
  //test("pairs 0") {
  //}

}
