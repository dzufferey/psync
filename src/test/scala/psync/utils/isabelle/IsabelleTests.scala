package psync.utils.isabelle

import psync.formula._
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

  //TODO proving some simple theorem
  // (∀ x. P x) ⇒ P y
  
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
      prove("exluded middle", Or(x, Not(x)), "x \\<or> \\<not> x")
      val f2 = Eq(Plus(Literal(0),Literal(1),Literal(1)), Literal(2))
      prove("addition", Eq(Plus(Literal(0),Literal(1),Literal(1)), Literal(2)), "0 + (Suc 0 + Suc 0) = Suc (Suc 0)")
      val a = UnInterpreted("a")
      val y = Variable("y").setType(a)
      prove("id", Eq(y, y), "y = y")
      prove("defined some", IsDefined(FSome(y)), "\\<not> Option.is_none (Some y)")
      prove("some", Neq(FSome(y), FNone()), "Some y \\<noteq> None")
      prove("get some", Eq(Get(FSome(y)), y), "the (Some y) = y")
      val z = Variable("z").setType(a)
      def p(f: Formula) = UnInterpretedFct("P", Some(a ~> Bool), Nil)(f)
      prove("(∀ y. P y) ⇒ P z", Implies(ForAll(List(y), p(y)), p(z)), "(\\<forall>y. P y) \\<longrightarrow> P z")
      def q(f: Formula, g: Formula) = UnInterpretedFct("Q")(f, g)
      prove("(∀ y z. Q y z) ⇒ Q y z", Implies(ForAll(List(y,z), q(y,z)), q(y,z)), "(\\<forall>y z. Q y z) \\<longrightarrow> Q y z")
    } finally {
      s.stop
    }
  }

}
