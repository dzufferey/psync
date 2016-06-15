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

  test("proving simple theorems") {
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

}
