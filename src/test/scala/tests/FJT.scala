package tests

import java.util.concurrent.ForkJoinTask

//  check if it induce a memory barrier
//      -> seems to work
//  checks what happens if we join on a task that is not yet forked
//      -> forking from the end of the queue is much slower but it works

object FJT {

  var b: Boolean = true

  class Swapper(expected: Boolean, previous: ForkJoinTask[Unit]) extends ForkJoinTask[Unit] {
    protected def exec = { 
      if (previous != null) previous.join
      assert(b == expected)
      b = !b
      true
    }
    protected def setRawResult(u: Unit) { }
    protected def getRawResult = ()
  }

  def main(args: Array[java.lang.String]) {
    val nbr = args(0).toInt
    println("queuing " + nbr + " tasks")
    var prev: ForkJoinTask[Unit] = null
    val tasks = for (i <- 0 until nbr) yield {
      val s = new Swapper(i % 2 == 0, prev)
      prev = s
      s
    }
    tasks.foreach(_.fork)
    //tasks.foldRight( () )( (t, acc) => t.fork )
    prev.join
  }

}
