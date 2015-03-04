package round.logic

import round.formula._
import round.utils.smtlib._

import org.scalatest._

class VennRegionsSuite extends FunSuite {
  
  //TODO test the constraints
  //TODO comprehensions to test the membership axioms

  val pid = CL.procType
  val n = CL.n

  val a = Variable("A").setType(FSet(pid))
  val b = Variable("B").setType(FSet(pid))
  val c = Variable("C").setType(FSet(pid))

  test("0 set") {
    val vr = new VennRegions(pid, Some(n), Nil)
    assert(vr.getVennRegion(Nil).name.endsWith("_"))
    assert(vr.ennumerate.toList.length == 1)
    vr.sumToUniverse match {
      case Eq(n, Variable(_)) => ()
      case other => assert(false, other)
    }
  }
  
  test("no universe size") {
    val vr = new VennRegions(pid, None, Nil)
    vr.sumToUniverse match {
      case True() => ()
      case other => assert(false, other)
    }
  }

  test("1 set") {
    val vr = new VennRegions(pid, Some(n), List(a -> None))
    assert(vr.getVennRegion(a).name.endsWith("t"))
    assert(vr.getVennRegion(Not(a)).name.endsWith("f"))
    assert(vr.getVennRegion(Nil).name.endsWith("_"))
    assert(vr.ennumerate.toList.length == 2)
    vr.sumToUniverse match {
      case Eq(n, Plus(lst @ _*)) if lst.size == 2 => ()
      case other => assert(false, other)
    }
  }

  test("2 sets") {
    val vr = new VennRegions(pid, Some(n), List(a -> None, b -> None))
    assert(vr.getVennRegion(a).name.endsWith("_"))
    assert(vr.getVennRegion(b).name.endsWith("t"))
    assert(vr.getVennRegion(Not(b)).name.endsWith("f"))
    assert(vr.getVennRegion(Nil).name.endsWith("_"))
    assert(vr.ennumerate.toList.length == 4)
    vr.sumToUniverse match {
      case Eq(n, Plus(lst @ _*)) if lst.size == 4 => ()
      case other => assert(false, other)
    }
  }

  test("3 sets") {
    val vr = new VennRegions(pid, Some(n), List(a -> None, b -> None, c -> None))
    assert(vr.getVennRegion(a).name.endsWith("_"))
    assert(vr.getVennRegion(b).name.endsWith("_"))
    assert(vr.getVennRegion(c).name.endsWith("t"))
    assert(vr.getVennRegion(Not(c)).name.endsWith("f"))
    assert(vr.getVennRegion(Nil).name.endsWith("_"))
    assert(vr.ennumerate.toList.length == 8)
    vr.sumToUniverse match {
      case Eq(n, Plus(lst @ _*)) if lst.size == 8 => ()
      case other => assert(false, other)
    }
  }

  test("bound") {
    val c1 = List(a -> None)
    val c2 = List(a -> None, b -> None)
    val c3 = List(a -> None, b -> None, c -> None)
    def mk(b: Int, lst: List[(Formula, Option[Binding])]): List[Formula] = {
      val vr = new VennRegionsWithBound(b, pid, Some(n), lst)
      FormulaUtils.getConjuncts(vr.constraints)
    }
    assert(mk(1, c1).size == 1)
    assert(mk(1, c2).size == 2)
    assert(mk(1, c3).size == 3)
    assert(mk(2, c1).size == 1)
    assert(mk(2, c2).size == 1)
    assert(mk(2, c3).size == 3)
    assert(mk(3, c1).size == 1)
    assert(mk(3, c2).size == 1)
    assert(mk(3, c3).size == 1)
  }

}
