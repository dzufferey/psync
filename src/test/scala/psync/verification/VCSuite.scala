package psync.verification

import psync.formula._
import psync.formula.InlineOps._
import psync.logic.CL
import psync.logic.TestCommon._
import psync.logic.quantifiers.Eager

import org.scalatest._

class VCSuite extends FunSuite {

  test("Tcp") {

    val coord = Variable("coord").setType(pid)
    val v = Variable("v").setType(Bool)
    val s = Variable("s").setType(Int)
    val mailbox = UnInterpretedFct("mailbox", Some(pid ~> FMap(pid, Bool)))
    val data0 = UnInterpretedFct("data0",Some(pid ~> Bool))
    val data  = UnInterpretedFct("data", Some(pid ~> Bool))
    val data1 = UnInterpretedFct("data1",Some(pid ~> Bool))
    val decided  = UnInterpretedFct("decided", Some(pid ~> Bool))
    val decided1 = UnInterpretedFct("decided1",Some(pid ~> Bool))
    val vote  = UnInterpretedFct("vote", Some(pid ~> Bool))
    val vote1 = UnInterpretedFct("vote1",Some(pid ~> Bool))
 
    val primeMap = Map[Symbol,Symbol](
      data -> data1,
      decided -> decided1,
      vote -> vote1
    )
    def prime(f: Formula) = FormulaUtils.mapSymbol( x => primeMap.getOrElse(x, x), f)
    
    val agreement = ForAll(List(i,j), Implies(decided(i) && decided(j), data(i) === data(j)))
    val validity = ForAll(List(i), Exists(List(j), Implies(decided(i) && data(i), data0(j))))
 
    val initialState = ForAll(List(i), Not(vote(i)) && Not(decided(i)))
 
    val round1 = And(
      //send, mailbox
      ForAll(List(i, j), And(
          IsDefinedAt(mailbox(i),j) === (Eq(i, coord) && In(j, ho(i))),
          LookUp(mailbox(i), j) === data0(j)
        )
      ),
      //update
      ForAll(List(i), And(
        //coord
        Implies(i === coord,
          Exists(List(s), And(
            s === Comprehension(List(j), IsDefinedAt(mailbox(i), j) && LookUp(mailbox(i), j) === True()).card,
            Implies(s === n, vote1(i)),
            Implies(s !== n, Not(vote1(i)))
        ))),
        //non coord
        Implies(i !== coord, vote(i) === vote1(i)),
        //frame
        data(i) === data1(i),
        decided(i) === decided1(i)
      ))
    )
 
    val round2 = And(
      //send, mailbox
      ForAll(List(i,j), And(
          IsDefinedAt(mailbox(i), j) === (Eq(j, coord) && In(j, ho(i))),
          LookUp(mailbox(i), j) === vote(j)
        )
      ),
      //update
      ForAll(List(i), And(
        Or(
          IsDefinedAt(mailbox(i), coord) && decided1(i) && Eq(data1(i), LookUp(mailbox(i), coord)),
          Not(IsDefinedAt(mailbox(i), coord)) && Not(decided1(i)) && Eq(data1(i), data(i))
        ),
        Eq(vote(i),vote1(i))
      ))
    )
 
    val invariant1 = ForAll(List(i), And(
      Implies(vote(coord), data0(i)),
      Implies(decided(i), Eq(data(i), vote(coord)))
    ))
    val invariantP = prime(invariant1)

    val cl = new CL(cln(1, new Eager,  1, true))

    val vcs = Seq(
      new SingleVC("invariant implies agreement", invariant1, True(), agreement, Nil, cl),
      new SingleVC("invariant implies validity", invariant1, True(), validity, Nil, cl),
      new SingleVC("initialState and round 1 implies invariant 1", initialState, round1, invariantP, Nil, cl),
      new SingleVC("invariant 1 is inductive at round 2", invariant1, round2, invariantP, Nil, cl)
    )

    vcs.foreach( v => {
      assert(v.isValid)
      assert(v.decompose.isValid)
    })
  }

}
