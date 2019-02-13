package psync.logic

import psync.formula._
import psync.formula.InlineOps._
import TestCommon._

import org.scalatest._

class MultiPraxosMboxAxioms extends FunSuite {

  val p = Variable("p").setType(pid)
  val q = Variable("q").setType(pid)

  // during broadcast, one leader known to all
  val leader = Variable("leader").setType(pid)
  val π = Variable("pi").setType(FSet(pid)) // set of all processes

  // set of active processes
  val act = Variable("act").setType(FSet(pid))
  val act1 = Variable("act1").setType(FSet(pid))

  // log-related types & uninterpreted functions
  val commandType = UnInterpreted("command")
  val committedType = Bool
  val logEntryType = Product(commandType, committedType)
  val key = Int

  val i = Variable("i").setType(key)

  val log        = UnInterpretedFct("log", Some(pid ~> FMap(key, logEntryType)))
  val log1       = UnInterpretedFct("log1", Some(pid ~> FMap(key, logEntryType)))
  val lastIndex  = UnInterpretedFct("lastIndex", Some(pid ~> key))
  val lastIndex1 = UnInterpretedFct("lastIndex1", Some(pid ~> key))

  val payloadType   = UnInterpreted("payload")

  val send = UnInterpretedFct("send", Some(pid ~> FMap(pid, commandType)))
  val mbox = UnInterpretedFct("mbox", Some(pid ~> FMap(pid, commandType)))

  // right now the type of send is too permissive
  // perhaps we could consider the following type
  // val send = UnInterpretedFct("send", Some(pid ~> Product(payloadType, FSet(pid))))
  // (i.e we can only send one msg at each roung -- to many processes)

  // TODO: describe each axiom
  val axioms = And(
    // π is the set of all processes
    ForAll(List (p), (p ∈ π)),

    ForAll(List (p), (KeySet(mbox(p)) ⊆ π)),
    ForAll(List (p), (KeySet(send(p)) ⊆ π)),
    ForAll(List (p), ((ho(p)) ⊆ π)),
    // all processes in ho are in the keyset of the mbox
    // and the converse is true (keyset of send)

    //
    // ForAll(List (p, q),
    //   ((p ∈ KeySet(send(q)))) ==> (q ∈ KeySet(mbox(p)))),
    //
    // ForAll(List (p, q),
    //   (q ∈ KeySet(mbox(p))) ==> ((p ∈ KeySet(send(q))))),

    ForAll(List (p, q),
      ((q ∈ ho(p) ∧ (p ∈ KeySet(send(q)))) ==> (q ∈ KeySet(mbox(p))))),

    ForAll(List (p, q),
      (q ∈ KeySet(mbox(p))) ==> ((q ∈ ho(p) ∧ (p ∈ KeySet(send(q)))))),

    // ForAll(List (p, q),
    //   (q ∈ ho(p) ∧ (p ∈ KeySet(send(q)))) ==> (IsDefinedAt(mbox(p), q) ∧ IsDefinedAt(send(q), p))),

    // ForAll(List (p, q),
    //   Or((q ∉ ho(p)), p ∉ KeySet(send(q)))==> (Not(IsDefinedAt(mbox(p), q)))),

    // ForAll(List (p, q),
    //   (Not(IsDefinedAt(mbox(p), q))) ==> Or(((q ∉ ho(p))), p ∉ KeySet(send(q)))),

    // ForAll(List (p, q),
    //   (p ∉ KeySet(send(q))) ==> (Not(q ∈ KeySet(mbox(p))))),

    // // process p receives from q what q sent to p
    // ForAll(List (p, q), ((q ∈ ho(p) ) ∧ (p ∈ KeySet(send(q)))) ==> (mbox(p).lookUp(q) === send(q).lookUp(p))),

    //ForAll(List(p), SubsetEq(KeySet(mbox(p)), ho(p))),
    ForAll(List(p), KeySet(mbox(p)).card <= n),
    ForAll(List(p), KeySet(mbox(p)).card >= 0),
    ForAll(List(p), (ho(p)).card <= n),
    ForAll(List(p), (ho(p)).card >= n),
    π.card === n,
    KeySet(send(leader)).card === n,
    // non-leaders do not send anything
    ForAll(List(p), (p ≠ leader) ==> (KeySet(send(p)).card === 0)),

    ForAll(List (p), (p ∈ KeySet(send(leader))))
  )


  val lmbox = Exists(List (p),
     (KeySet(mbox(p)).card > 0) ==> (leader ∉ KeySet(mbox(p))))

  test("test"){
    val fs = List(
      axioms,
      lmbox
    )
    assertUnsat(fs,c2e1)
    //val reducer = c2e1
    //assertUnsat(fs, debug=false, to=60000, reducer=reducer)
    // // val f0 = reduce(c2e1, fs, true, true)
    //  getModel(fs, to=60000, reducer=reducer)
  }


}
