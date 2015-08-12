package round.logic

import round.formula._
import round.formula.InlineOps._
import round.utils.smtlib._
import dzufferey.utils.Logger
import TestCommon._

import org.scalatest._

//an example about memory allocation from Shaz Qadeer
class ShazExample extends FunSuite {

  val memLo = Variable("memLo").setType(Int)
  val memHi = Variable("memHi").setType(Int)

  val memAssumptions = And(memLo > 0, memLo <= memHi)

  val loc = Variable("loc").setType(Int)
  val l1  = Variable("loc1").setType(Int)
  val l2  = Variable("loc2").setType(Int)

  val memAddr = Variable("memAddr").setType(FSet(Int))
  //memAddr = {i. memLo ≤ i ∧ i < memHi}
  val memAddrDef = memAddr === Comprehension(List(loc), (memLo <= loc) && (loc < memHi) )

  val tid = Variable("tid").setType(pid)
  val mutator = Variable("mutator").setType(FSet(pid))
  val gcPid = Variable("gcPid").setType(pid)
  val threadAssumptions = (mutator.card > 0) && Not(gcPid ∈ mutator)

  val free = Variable("free").setType(FSet(Int)) // Set[mem loc]
  val freeSpace = Variable("freeSpace").setType(Int)
  val allocatingAtOrAfter = Variable("allocatingAtOrAfter").setType(FMap(Int,FSet(pid)))
  val numFreeAtOrAfter = Variable("numFreeAtOrAfter").setType(FMap(Int, Int)) // mem loc → int

  val free1 = Variable("free1").setType(FSet(Int)) // Set[mem loc]
  val freeSpace1 = Variable("freeSpace1").setType(Int)
  val allocatingAtOrAfter1 = Variable("allocatingAtOrAfter1").setType(FMap(Int,FSet(pid)))
  val numFreeAtOrAfter1 = Variable("numFreeAtOrAfter1").setType(FMap(Int, Int)) // mem loc → int

  def invariant( numFreeAtOrAfter: Formula,
                 allocatingAtOrAfter: Formula,
                 free: Formula,
                 freeSpace: Formula): Formula = {    
    def aaoa(f: Formula) = LookUp(allocatingAtOrAfter, f)
    def nfaoa(f: Formula) = LookUp(numFreeAtOrAfter, f)
    And(
      (aaoa(memLo).card + freeSpace) === nfaoa(memLo),
      ForAll(List(loc), aaoa(loc).card <= nfaoa(loc) ),
      freeSpace <= 0,
      ForAll(List(l1, l2), Implies(And(l1 ∈ memAddr, l2 ∈ memAddr, l1 <= l2), aaoa(l1) ⊆ aaoa(l2))),
      ForAll(List(loc), Or(loc ∈ memAddr, nfaoa(loc) === 0)),
      ForAll(List(loc), Implies(And(loc ∈ memAddr,     loc ∈ free),  Eq(nfaoa(loc), nfaoa(loc + 1) + 1))),
      ForAll(List(loc), Implies(And(loc ∈ memAddr, Not(loc ∈ free)), Eq(nfaoa(loc), nfaoa(loc + 1))))
    )
  }

  //TODO be more considerate in the quantifier inst. this really blows up.
//test("Reclaim") {
//  val fs = List(
//    memAssumptions,
//    memAddrDef,
//    threadAssumptions,
//    tid === gcPid,
//    invariant(numFreeAtOrAfter, allocatingAtOrAfter, free, freeSpace),
//    loc ∈ memAddr,
//    Not(loc ∈ free),
//    freeSpace1 === (freeSpace + 1),
//    free1 === Comprehension(List(l1), (l1 ∈ free) || l1 === loc),
//    ForAll(List(l1), ite( And(memLo <= l1, l1 <= loc),
//                          numFreeAtOrAfter1.lookUp(l1) === (numFreeAtOrAfter.lookUp(l1) + 1),
//                          numFreeAtOrAfter1.lookUp(l1) === numFreeAtOrAfter.lookUp(l1))
//    ),
//    Not(invariant(numFreeAtOrAfter1, allocatingAtOrAfter, free1, freeSpace1))
//  )
//  assertUnsat(fs, 10000, true)
//}


  //TODO Malloc(tid: X) part 1
  //assume mutator(tid);
  //assume invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);    
  //assume (forall u: int :: !AllocatingAtOrAfter[u][tid]);
  //var i: int;
  //var spaceFound: bool;
  ////YieldAlloc(tid, 0) precond
  //assert mutator(tid);
  //assert Invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assert (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= 0);

  //TODO Malloc(tid: X) part 2
  //assume mutator(tid);
  //assume Invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= 0);
  //assert (forall v: int :: memAddr(v) ==> Subset(AllocatingAtOrAfter[v], AllocatingAtOrAfter[memLo]));

  //TODO Malloc(tid: X) part 3
  //assume mutator(tid);
  //assume Invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= 0);
  //assume (forall v: int :: memAddr(v) ==> Subset(AllocatingAtOrAfter[v], AllocatingAtOrAfter[memLo]));
  ////DecrementFreeSpace(tid) precond
  //assert AllocatingAtOrAfter[memLo] == AllocatingAtOrAfter[memLo][tid := false];

  //TODO Malloc(tid: X) part 4
  //assume mutator(tid);
  //assume Invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= 0);
  //assume (forall v: int :: memAddr(v) ==> Subset(AllocatingAtOrAfter[v], AllocatingAtOrAfter[memLo]));
  ////DecrementFreeSpace(tid)
  //assume AllocatingAtOrAfter[memLo] == AllocatingAtOrAfter[memLo][tid := false];
  //assume 0 < freeSpace; 
  //freeSpace := freeSpace - 1; 
  //AllocatingAtOrAfter[memLo][tid] := true;
  //i := memLo;
  ////YieldAlloc(tid, i) precond
  //assert mutator(tid);
  //assert Invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assert (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);

  //TODO Malloc(tid: X) part 5
  //i := memLo;
  //assume mutator(tid);
  //assume Invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  ////loop precond
  //assert memAddr(i);
  //assert invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assert (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);

  //TODO Malloc(tid: X) part 6
  //assume mutator(tid);
  //assume memAddr(i);
  //assume invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  //assume i < memHi
  ////first assert
  //assert memAddr(i+1) ==> (forall v: int :: memAddr(v) && i+1 <= v ==> Subset(AllocatingAtOrAfter[v], AllocatingAtOrAfter[i+1]));

  //TODO Malloc(tid: X) part 7
  //assume mutator(tid);
  //assume memAddr(i);
  //assume invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  //assume i < memHi
  ////second assert
  //assert memAddr(i+1) ==> (forall v: int :: memAddr(v) && v <= i+1 ==> Subset(AllocatingAtOrAfter[i+1], AllocatingAtOrAfter[v]));

  //TODO Malloc(tid: X) part 8
  //assume mutator(tid);
  //assume memAddr(i);
  //assume invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  //assume i < memHi
  //assume memAddr(i+1) ==> (forall v: int :: memAddr(v) && i+1 <= v ==> Subset(AllocatingAtOrAfter[v], AllocatingAtOrAfter[i+1]));
  //assume memAddr(i+1) ==> (forall v: int :: memAddr(v) && v <= i+1 ==> Subset(AllocatingAtOrAfter[i+1], AllocatingAtOrAfter[v]));
  ////spaceFound := AllocIfPtrFree(tid, i); precond
  //assert memAddr(i);
  //assert Free[ptr] || memAddr(i + 1);
  //assert (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);

  //TODO Malloc(tid: X) part 9
  //assume mutator(tid);
  //assume memAddr(i);
  //assume invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  //assume i < memHi
  //assume memAddr(i+1) ==> (forall v: int :: memAddr(v) && i+1 <= v ==> Subset(AllocatingAtOrAfter[v], AllocatingAtOrAfter[i+1]));
  //assume memAddr(i+1) ==> (forall v: int :: memAddr(v) && v <= i+1 ==> Subset(AllocatingAtOrAfter[i+1], AllocatingAtOrAfter[v]));
  //assume Free[ptr] || memAddr(i + 1);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  ////AllocIfPtrFree body
  //Or( assume (spaceFound);
  //    Free[ptr] := false;
  //    NumFreeAtOrAfter := (lambda u: int :: NumFreeAtOrAfter[u] - (if memLo <= u && u <= ptr then 1 else 0));
  //    AllocatingAtOrAfter := (lambda u: int :: AllocatingAtOrAfter[u][tid := false]);
  //  ,
  //    assume (!spaceFound);
  //    AllocatingAtOrAfter[ptr+1][tid] := true;
  //  )
  ////loop body YieldAlloc(tid, 0) precond
  //assume (spaceFound);
  //assert mutator(tid);
  //assert Invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assert (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= 0);

  //TODO Malloc(tid: X) part 10
  //assume mutator(tid);
  //assume memAddr(i);
  //assume invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  //assume i < memHi
  //assume memAddr(i+1) ==> (forall v: int :: memAddr(v) && i+1 <= v ==> Subset(AllocatingAtOrAfter[v], AllocatingAtOrAfter[i+1]));
  //assume memAddr(i+1) ==> (forall v: int :: memAddr(v) && v <= i+1 ==> Subset(AllocatingAtOrAfter[i+1], AllocatingAtOrAfter[v]));
  //assume Free[ptr] || memAddr(i + 1);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  ////AllocIfPtrFree body
  //Or( assume (spaceFound);
  //    Free[ptr] := false;
  //    NumFreeAtOrAfter := (lambda u: int :: NumFreeAtOrAfter[u] - (if memLo <= u && u <= ptr then 1 else 0));
  //    AllocatingAtOrAfter := (lambda u: int :: AllocatingAtOrAfter[u][tid := false]);
  //  ,
  //    assume (!spaceFound);
  //    AllocatingAtOrAfter[ptr+1][tid] := true;
  //  )
  ////loop body YieldAlloc(tid, i) precond
  //assume (!spaceFound);
  //i := i + 1
  //assert mutator(tid);
  //assert Invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assert (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);

  //TODO Malloc(tid: X) part 11
  //assume mutator(tid);
  //assume memAddr(i);
  //assume invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  //assume i < memHi
  //assume memAddr(i+1) ==> (forall v: int :: memAddr(v) && i+1 <= v ==> Subset(AllocatingAtOrAfter[v], AllocatingAtOrAfter[i+1]));
  //assume memAddr(i+1) ==> (forall v: int :: memAddr(v) && v <= i+1 ==> Subset(AllocatingAtOrAfter[i+1], AllocatingAtOrAfter[v]));
  //assume Free[ptr] || memAddr(i + 1);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  ////AllocIfPtrFree body
  //Or( assume (spaceFound);
  //    Free[ptr] := false;
  //    NumFreeAtOrAfter := (lambda u: int :: NumFreeAtOrAfter[u] - (if memLo <= u && u <= ptr then 1 else 0));
  //    AllocatingAtOrAfter := (lambda u: int :: AllocatingAtOrAfter[u][tid := false]);
  //  ,
  //    assume (!spaceFound);
  //    AllocatingAtOrAfter[ptr+1][tid] := true;
  //  )
  ////loop body
  //Or( assume (spaceFound);
  //    Free[ptr] := false;
  //    assume mutator(tid);
  //    assume Invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //    assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= 0);
  //    assume false; //return
  //  ,
  //    assume (!spaceFound);
  //    i := i + 1
  //    assert mutator(tid);
  //    assert Invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //    assert (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  //  )
  ////loop postcond 
  //assert memAddr(i);
  //assert invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assert (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);

  //TODO Malloc(tid: X) part 12
  //assume mutator(tid);
  //assume memAddr(i);
  //assume invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  //assume i < memHi
  //assume memAddr(i+1) ==> (forall v: int :: memAddr(v) && i+1 <= v ==> Subset(AllocatingAtOrAfter[v], AllocatingAtOrAfter[i+1]));
  //assume memAddr(i+1) ==> (forall v: int :: memAddr(v) && v <= i+1 ==> Subset(AllocatingAtOrAfter[i+1], AllocatingAtOrAfter[v]));
  //assume Free[ptr] || memAddr(i + 1);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  ////AllocIfPtrFree body
  //Or( assume (spaceFound);
  //    Free[ptr] := false;
  //    NumFreeAtOrAfter := (lambda u: int :: NumFreeAtOrAfter[u] - (if memLo <= u && u <= ptr then 1 else 0));
  //    AllocatingAtOrAfter := (lambda u: int :: AllocatingAtOrAfter[u][tid := false]);
  //  ,
  //    assume (!spaceFound);
  //    AllocatingAtOrAfter[ptr+1][tid] := true;
  //  )
  ////loop body
  //assume (spaceFound);
  //Free[ptr] := false;
  //assume mutator(tid);
  //assume invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= 0);
  ////postcond return inside loop
  //assert invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);    
  //assert (forall u: int :: !AllocatingAtOrAfter[u][tid]);

  //TODO Malloc(tid: X) part 13
  //assume memAddr(i);
  //assume invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);
  //assume (forall u: int :: AllocatingAtOrAfter[u][tid] <==> memLo <= u && u <= i);
  ////exiting loop
  //assume i >= memHi
  ////postcond return inside loop
  //assert invariant(NumFreeAtOrAfter, AllocatingAtOrAfter, Free, freeSpace);    
  //assert (forall u: int :: !AllocatingAtOrAfter[u][tid]);

}
