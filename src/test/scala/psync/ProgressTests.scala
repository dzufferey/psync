package psync

import org.scalatest._
import prop._

class ProgressTests extends FunSuite with PropertyChecks {

  test("timeout id") {
    forAll { (l: Long) =>
      assert(!Progress.timeoutInBounds(l) || Progress.timeout(l).timeout == l)
    }
  }

  test("strict timeout id") {
    forAll { (l: Long) =>
      assert(!Progress.timeoutInBounds(l) || Progress.strictTimeout(l).timeout == l)
    }
  }
  
  test("timeout strictness") {
    forAll { (l: Long) =>
      assert(!Progress.timeout(l).isStrict)
    }
  }
  
  test("strict timeout strictness") {
    forAll { (l: Long) =>
      assert(Progress.strictTimeout(l).isStrict)
    }
  }

  test("sanity checks for bounds") {
    assert(Progress.timeoutInBounds(0))
    assert(Progress.timeoutInBounds(10))
    assert(Progress.timeoutInBounds(100))
    assert(Progress.timeoutInBounds(1000))
    assert(Progress.timeoutInBounds(10000))
    assert(Progress.timeoutInBounds(100000))
  }
  
  test("sanity checks for wait") {
    val w = Progress.waitMessage
    val ws = Progress.strictWaitMessage
    assert(w.isWaitMessage)
    assert(!w.isStrict)
    assert(ws.isWaitMessage)
    assert(ws.isStrict)
    assert(!w.isUnchanged)
    assert(!w.isTimeout)
    assert(!w.isGoAhead)
    assert(!ws.isUnchanged)
    assert(!ws.isTimeout)
    assert(!ws.isGoAhead)
  }
  
  test("sanity checks for unchanged") {
    assert(Progress.unchanged.isUnchanged)
    assert(!Progress.unchanged.isTimeout)
    assert(!Progress.unchanged.isGoAhead)
    assert(!Progress.unchanged.isWaitMessage)
  }
  
  test("sanity checks for goAhead") {
    assert(Progress.goAhead.isGoAhead)
    assert(!Progress.goAhead.isUnchanged)
    assert(!Progress.goAhead.isTimeout)
    assert(!Progress.goAhead.isWaitMessage)
  }


}

