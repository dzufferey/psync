package psync.utils

import psync.Time
import org.scalatest._

class LookaheadCounterTests extends FunSuite {
  
  test("creation") {
    assertThrows[AssertionError] { new LookaheadCounter(-1) }
    assertThrows[AssertionError] { new LookaheadCounter(0) }
    new LookaheadCounter(1)
    new LookaheadCounter(2)
    assertThrows[AssertionError] { new LookaheadCounter(3) }
    new LookaheadCounter(4)
    assertThrows[AssertionError] { new LookaheadCounter(5) }
    assertThrows[AssertionError] { new LookaheadCounter(6) }
    assertThrows[AssertionError] { new LookaheadCounter(7) }
    new LookaheadCounter(8)
  }

  test("ops") {
    val c = new LookaheadCounter(2)
    assert(c.get(new Time(0)) == 0)
    assert(c.get(new Time(1)) == 0)
    assert(c.get(new Time(2)) == 0)
    assert(c.get(new Time(123456)) == 0)
    assert(c.get(new Time(-1)) == 0)
    assert(c.increase(new Time(0)) == 1)
    assert(c.increase(new Time(0)) == 2)
    assert(c.increase(new Time(0)) == 3)
    assert(c.increase(new Time(0)) == 4)
    assert(c.increase(new Time(0)) == 5)
    assert(c.get(new Time(0)) == 5)
    assert(c.get(new Time(1)) == 0)
    assert(c.get(new Time(2)) == 0)
    assert(c.decrease(new Time(0)) == 4)
    assert(c.decrease(new Time(0)) == 3)
    assert(c.decrease(new Time(0)) == 2)
    assert(c.decrease(new Time(0)) == 1)
    assert(c.increase(new Time(2)) == 0)
    assert(c.getCurrent == 1)
    assert(c.increase(new Time(1)) == 1)
    assert(c.increase(new Time(1)) == 2)
    c.next
    assert(c.getCurrent == 2)
    assert(c.get(new Time(0)) == 0)
    assert(c.get(new Time(1)) == 2)
    assert(c.get(new Time(2)) == 0)
    c.reset
    assert(c.get(new Time(0)) == 0)
    assert(c.get(new Time(1)) == 0)
    assert(c.get(new Time(2)) == 0)
  }

}
