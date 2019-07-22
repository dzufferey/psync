package psync.utils

import org.scalatest._

class LongBitSetTests extends FunSuite {
  
  test("full/empty") {
    for (i <- 0 until 64) {
      assert(!LongBitSet.empty.get(i))
      assert(LongBitSet.full.get(i))
    }
  }

  test("set") {
    for (i <- 0 until 64) {
      assert(LongBitSet.empty.set(i).get(i))
      for(j <- 0 until 64 if j != i) {
        assert(!LongBitSet.empty.set(i).get(j), "i: " + i + ", j: " + j)
      }
    }
  }
  
  test("clear") {
    for (i <- 0 until 64) {
      assert(!LongBitSet.full.clear(i).get(i))
      for(j <- 0 until 64 if j != i) {
        assert(LongBitSet.full.clear(i).get(j), "i: " + i + ", j: " + j)
      }
    }
  }
  
  test("size") {
    assert(LongBitSet.empty.size == 0)
    assert(LongBitSet.full.size == 64)
    assert(LongBitSet.empty.set(1).set(32).set(64).size == 3)
    assert(LongBitSet.empty.set(1).set(32).set(65).size == 2)
  }

}
