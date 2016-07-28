package psync.utils.isabelle

import psync.formula._
import psync.formula.InlineOps._
import edu.tum.cs.isabelle.ProverResult._
import org.scalatest._

class IsabelleTests extends FunSuite {

  //import dzufferey.utils.Logger
  //Logger.moreVerbose
  //Logger.moreVerbose

  test("Session: start, hello, and stop") {
    val s = new Session
    s.start
    val h = s.hello
    assert(h == "Hello world")
    s.stop
  }

  test("proving simple theorems 1") {
    val s = new Session
    s.start
    def prove(name: String, f: Formula, expected: String) {
      s.lemma(name, Nil, f, None) match {
        case Success(Some(str)) => assert(str == expected)
        case other => sys.error(other.toString)
      }
    }
    try {
      s.newTheory("Test1")
      val x = Variable("x").setType(Bool)
      prove("x ∨ !x", x ∨ Not(x), "x \\<or> \\<not> x")
      prove("0+1+1 = 2", Plus(0, 1, 1) === 2, "0 + (Suc 0 + Suc 0) = Suc (Suc 0)")
      val a = UnInterpreted("a")
      val y = Variable("y").setType(a)
      prove("y = y", y === y, "y = y")
      prove("defined some", IsDefined(FSome(y)), "\\<not> Option.is_none (Some y)")
      prove("some", FSome(y) !== FNone(), "Some y \\<noteq> None")
      prove("get some", Get(FSome(y)) === y, "the (Some y) = y")
      val z = Variable("z").setType(a)
      def p(f: Formula) = UnInterpretedFct("P", Some(a ~> Bool), Nil)(f)
      prove("(∀ y. P y) ⇒ P z", Implies(ForAll(List(y), p(y)), p(z)), "(\\<forall>y. P y) \\<longrightarrow> P z")
      def q(f: Formula, g: Formula) = UnInterpretedFct("Q")(f, g)
      prove("(∀ y z. Q y z) ⇒ Q y z", Implies(ForAll(List(y,z), q(y,z)), q(y,z)), "(\\<forall>y z. Q y z) \\<longrightarrow> Q y z")
      val i = Variable("i").setType(Int)
      val S = Variable("S").setType(FSet(a))
      prove("S = {y. ⊤} ⇒ y ∈ S", (S === Comprehension(List(y), True())) ==> y ∈ S, "S = {y. True} \\<longrightarrow> y \\<in> S")
      prove("z ∈ {y. ⊤}", z ∈ Comprehension(List(y), True()), "z \\<in> {y. True}")
      prove("1 ∈ {i. i ≥ 0}", Literal(1) ∈ Comprehension(List(i), i ≥ 0), "Suc 0 \\<in> {i. 0 \\<le> i}")
      prove("(x,y)._1 = x", Tuple(x,y)._1 === x, "fst (x, y) = x")
      prove("(x,y)._2 = y", Tuple(x,y)._2 === y, "snd (x, y) = y")
      prove("(x,y,z)._1 = x", Tuple(x,y,z)._1 === x, "fst (x, y, z) = x")
      prove("(x,y,z)._2 = y", Tuple(x,y,z)._2 === y, "fst (snd (x, y, z)) = y")
      prove("(x,y,z)._3 = z", Tuple(x,y,z)._3 === z, "snd (snd (x, y, z)) = z")
    } finally {
      s.stop
    }
  }

  test("proving simple theorems 2") {
    val s = new Session
    s.start
    val a = UnInterpreted("a")
    def prove(name: String, f: Formula, expected: String) {
      s.lemmaWithFiniteUniverse(name, List(a), Map.empty, Nil, f, None) match {
        case Success(Some(str)) => assert(str == expected)
        case other => sys.error(other.toString)
      }
    }
    try {
      s.newTheory("Test2")
      val x = Variable("x").setType(a)
      val S = Variable("S").setType(FSet(a))
      prove("|S| ≥ 0", S.card ≥ 0 , "finite UNIV \\<longrightarrow> 0 \\<le> card S")
      prove("|{}| = 0", Comprehension(List(x), False()).card === 0 , "finite UNIV \\<longrightarrow> card {x. False} = 0")
    } finally {
      s.stop
    }
  }

  test("proving simple theorems 3") {
    val s = new Session
    s.start
    val a = UnInterpreted("a")
    val x = Variable("x").setType(a)
    val n = Variable("n").setType(Int)
    def prove(name: String, hyps: Seq[Formula], concl: Formula, proof: Option[String], expected: String) {
      s.lemmaWithFiniteUniverse(name, List(a), Map(n -> a), hyps, concl, proof) match {
        case Success(Some(str)) => assert(str == expected)
        case other => sys.error(other.toString)
      }
    }
    try {
      s.newTheory("Test3")
      ///////////////
      val S = Variable("S").setType(FSet(a))
      val expected0 = "finite UNIV \\<and> n = card UNIV \\<longrightarrow> 0 \\<le> card S"
      prove("|S| ≥ 0", Nil, S.card ≥ 0, None, expected0)
      ///////////////
      val pred = UnInterpretedFct("P", Some(a ~> Bool))
      val p = Comprehension(List(x), pred(x))
      val np = Comprehension(List(x), Not(pred(x)))
      val proof1 = Some("(auto, tryAddVennFacts, linarith)")
      val expected1 = "finite UNIV \\<and> n = card UNIV \\<and> card {x. P x} = n \\<longrightarrow> card {x. \\<not> P x} = 0"
      prove("complement card", List(p.card === n), np.card === 0, proof1, expected1)
      ///////////////
      val f = UnInterpretedFct("f", Some(a ~> a))
      val cst1 = Variable("c1").setType(a)
      val cst2 = Variable("c2").setType(a)
      val hyp1 = (Comprehension(List(x), f(x) === cst1 ).card * 2) > n
      val hyp2 = (Comprehension(List(x), f(x) === cst2 ).card * 2) > n
      val concl = cst1 === cst2
      val proof2 = Some("(auto, rule ccontr, singleVennIntroNoForce \"{(x::'a). (f x) = c1}\" \"{(x::'a). (f x) = c2}\", force, tryAddVennFacts, simp)")
      val expected2 = "finite UNIV \\<and> n = card UNIV \\<and> n < card {x. f x = c1} * Suc (Suc 0) \\<and> n < card {x. f x = c2} * Suc (Suc 0) \\<longrightarrow> c1 = c2"
      prove("majority", List(hyp1, hyp2), concl, proof2, expected2)

    } finally {
      s.stop
    }
  }

}
