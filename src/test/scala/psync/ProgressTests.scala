package psync

import org.scalatest.funsuite.AnyFunSuite

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object ProgressProps extends Properties("Progress TO") {

  
  property("timeout id") = forAll { (l: Long) =>
    !Progress.timeoutInBounds(l) || Progress.timeout(l).timeout == l
  }

  property("strict timeout id") = forAll { (l: Long) =>
    !Progress.timeoutInBounds(l) || Progress.strictTimeout(l).timeout == l
  }
  
  property("timeout strictness") = forAll { (l: Long) =>
    !Progress.timeout(l).isStrict
  }
  
  property("strict timeout strictness") = forAll { (l: Long) =>
    Progress.strictTimeout(l).isStrict
  }

}

class ProgressTests extends AnyFunSuite {

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

  test("orElse") {
    def second(p: Progress) = Progress.unchanged.orElse(p) == p
    assert(second(Progress.unchanged))
    assert(second(Progress.goAhead))
    assert(second(Progress.waitMessage))
    assert(second(Progress.strictWaitMessage))
    assert(second(Progress.timeout(10)))
    assert(second(Progress.strictTimeout(10)))
    def first(p1: Progress, p2: Progress) = p1.orElse(p2) == p1
    assert(first(Progress.unchanged, Progress.unchanged))
    assert(first(Progress.goAhead, Progress.unchanged))
    assert(first(Progress.waitMessage, Progress.unchanged))
    assert(first(Progress.strictWaitMessage, Progress.unchanged))
    assert(first(Progress.timeout(10), Progress.unchanged))
    assert(first(Progress.strictTimeout(10), Progress.unchanged))
  }

  test("lub") {
    assert(Progress.lub(Progress.goAhead,           Progress.goAhead)           == Progress.goAhead)
    assert(Progress.lub(Progress.goAhead,           Progress.waitMessage)       == Progress.waitMessage)
    assert(Progress.lub(Progress.goAhead,           Progress.strictWaitMessage) == Progress.strictWaitMessage)
    assert(Progress.lub(Progress.goAhead,           Progress.timeout(10))       == Progress.timeout(10))
    assert(Progress.lub(Progress.goAhead,           Progress.strictTimeout(10)) == Progress.strictTimeout(10))
    assert(Progress.lub(Progress.timeout(10),       Progress.goAhead)           == Progress.timeout(10))
    assert(Progress.lub(Progress.timeout(10),       Progress.waitMessage)       == Progress.waitMessage)
    assert(Progress.lub(Progress.timeout(10),       Progress.strictWaitMessage) == Progress.strictWaitMessage)
    assert(Progress.lub(Progress.timeout(10),       Progress.timeout(10))       == Progress.timeout(10))
    assert(Progress.lub(Progress.timeout(10),       Progress.strictTimeout(10)) == Progress.strictTimeout(10))
    assert(Progress.lub(Progress.strictTimeout(10), Progress.goAhead)           == Progress.strictTimeout(10))
    assert(Progress.lub(Progress.strictTimeout(10), Progress.waitMessage)       == Progress.strictWaitMessage)
    assert(Progress.lub(Progress.strictTimeout(10), Progress.strictWaitMessage) == Progress.strictWaitMessage)
    assert(Progress.lub(Progress.strictTimeout(10), Progress.timeout(10))       == Progress.strictTimeout(10))
    assert(Progress.lub(Progress.strictTimeout(10), Progress.strictTimeout(10)) == Progress.strictTimeout(10))
    assert(Progress.lub(Progress.waitMessage,       Progress.goAhead)           == Progress.waitMessage)
    assert(Progress.lub(Progress.waitMessage,       Progress.waitMessage)       == Progress.waitMessage)
    assert(Progress.lub(Progress.waitMessage,       Progress.strictWaitMessage) == Progress.strictWaitMessage)
    assert(Progress.lub(Progress.waitMessage,       Progress.timeout(10))       == Progress.waitMessage)
    assert(Progress.lub(Progress.waitMessage,       Progress.strictTimeout(10)) == Progress.strictWaitMessage)
    assert(Progress.lub(Progress.strictWaitMessage, Progress.goAhead)           == Progress.strictWaitMessage)
    assert(Progress.lub(Progress.strictWaitMessage, Progress.waitMessage)       == Progress.strictWaitMessage)
    assert(Progress.lub(Progress.strictWaitMessage, Progress.strictWaitMessage) == Progress.strictWaitMessage)
    assert(Progress.lub(Progress.strictWaitMessage, Progress.timeout(10))       == Progress.strictWaitMessage)
    assert(Progress.lub(Progress.strictWaitMessage, Progress.strictTimeout(10)) == Progress.strictWaitMessage)
    assert(Progress.lub(Progress.timeout(20),       Progress.timeout(10))       == Progress.timeout(20))
    assert(Progress.lub(Progress.timeout(20),       Progress.strictTimeout(10)) == Progress.strictTimeout(20))
    assert(Progress.lub(Progress.timeout(10),       Progress.timeout(20))       == Progress.timeout(20))
    assert(Progress.lub(Progress.timeout(10),       Progress.strictTimeout(20)) == Progress.strictTimeout(20))
    assert(Progress.lub(Progress.strictTimeout(20), Progress.timeout(10))       == Progress.strictTimeout(20))
    assert(Progress.lub(Progress.strictTimeout(20), Progress.strictTimeout(10)) == Progress.strictTimeout(20))
    assert(Progress.lub(Progress.strictTimeout(10), Progress.timeout(20))       == Progress.strictTimeout(20))
    assert(Progress.lub(Progress.strictTimeout(10), Progress.strictTimeout(20)) == Progress.strictTimeout(20))
  }

  test("glb") {
    assert(Progress.glb(Progress.goAhead,           Progress.goAhead)           == Progress.goAhead)
    assert(Progress.glb(Progress.goAhead,           Progress.waitMessage)       == Progress.goAhead)
    assert(Progress.glb(Progress.goAhead,           Progress.strictWaitMessage) == Progress.goAhead)
    assert(Progress.glb(Progress.goAhead,           Progress.timeout(10))       == Progress.goAhead)
    assert(Progress.glb(Progress.goAhead,           Progress.strictTimeout(10)) == Progress.goAhead)
    assert(Progress.glb(Progress.timeout(10),       Progress.goAhead)           == Progress.goAhead)
    assert(Progress.glb(Progress.timeout(10),       Progress.waitMessage)       == Progress.timeout(10))
    assert(Progress.glb(Progress.timeout(10),       Progress.strictWaitMessage) == Progress.timeout(10))
    assert(Progress.glb(Progress.timeout(10),       Progress.timeout(10))       == Progress.timeout(10))
    assert(Progress.glb(Progress.timeout(10),       Progress.strictTimeout(10)) == Progress.timeout(10))
    assert(Progress.glb(Progress.strictTimeout(10), Progress.goAhead)           == Progress.goAhead)
    assert(Progress.glb(Progress.strictTimeout(10), Progress.waitMessage)       == Progress.timeout(10))
    assert(Progress.glb(Progress.strictTimeout(10), Progress.strictWaitMessage) == Progress.strictTimeout(10))
    assert(Progress.glb(Progress.strictTimeout(10), Progress.timeout(10))       == Progress.timeout(10))
    assert(Progress.glb(Progress.strictTimeout(10), Progress.strictTimeout(10)) == Progress.strictTimeout(10))
    assert(Progress.glb(Progress.waitMessage,       Progress.goAhead)           == Progress.goAhead)
    assert(Progress.glb(Progress.waitMessage,       Progress.waitMessage)       == Progress.waitMessage)
    assert(Progress.glb(Progress.waitMessage,       Progress.strictWaitMessage) == Progress.waitMessage)
    assert(Progress.glb(Progress.waitMessage,       Progress.timeout(10))       == Progress.timeout(10))
    assert(Progress.glb(Progress.waitMessage,       Progress.strictTimeout(10)) == Progress.timeout(10))
    assert(Progress.glb(Progress.strictWaitMessage, Progress.goAhead)           == Progress.goAhead)
    assert(Progress.glb(Progress.strictWaitMessage, Progress.waitMessage)       == Progress.waitMessage)
    assert(Progress.glb(Progress.strictWaitMessage, Progress.strictWaitMessage) == Progress.strictWaitMessage)
    assert(Progress.glb(Progress.strictWaitMessage, Progress.timeout(10))       == Progress.timeout(10))
    assert(Progress.glb(Progress.strictWaitMessage, Progress.strictTimeout(10)) == Progress.strictTimeout(10))
    assert(Progress.glb(Progress.timeout(20),       Progress.timeout(10))       == Progress.timeout(10))
    assert(Progress.glb(Progress.timeout(20),       Progress.strictTimeout(10)) == Progress.timeout(10))
    assert(Progress.glb(Progress.timeout(10),       Progress.timeout(20))       == Progress.timeout(10))
    assert(Progress.glb(Progress.timeout(10),       Progress.strictTimeout(20)) == Progress.timeout(10))
    assert(Progress.glb(Progress.strictTimeout(20), Progress.timeout(10))       == Progress.timeout(10))
    assert(Progress.glb(Progress.strictTimeout(20), Progress.strictTimeout(10)) == Progress.strictTimeout(10))
    assert(Progress.glb(Progress.strictTimeout(10), Progress.timeout(20))       == Progress.timeout(10))
    assert(Progress.glb(Progress.strictTimeout(10), Progress.strictTimeout(20)) == Progress.strictTimeout(10))
  }

}

