package psync.utils

import dzufferey.report._
import java.util.concurrent._
import java.util.concurrent.atomic._

class Stats {

  private val map = new ConcurrentHashMap[String, (AtomicInteger, AtomicLong)]()

  private val comments = new ConcurrentLinkedQueue[String]()
  
  def clear {
    map.clear()
    comments.clear()
  }
  
  def comment[A](what: String) = {
    comments.add(what)
  }

  def apply[A](what: String, fct: => A): A = {
    val start = java.lang.System.currentTimeMillis
    try {
      val result: A = fct //that should force the evaluation
      result
    } finally {
      val stop = java.lang.System.currentTimeMillis
      val delta = stop - start
      //store the result
      val (cnt, time) = 
        if (map.containsKey(what)) {
          map.get(what)
        } else {
          val c1 = (new AtomicInteger(), new AtomicLong())
          val c2 = map.putIfAbsent(what, c1)
          if (c2 == null) c1 else c2
        }
      cnt.incrementAndGet
      time.addAndGet(delta)
    }
  }

  override def toString = {
    val str = new java.io.StringWriter()
    val writer = new java.io.BufferedWriter(str)
    report.toText(writer)
    writer.close
    str.toString
  }

  private def tblIterator = {
    val entries = map.entrySet.iterator
    new Iterator[(String, Int, Long)] {
      def hasNext = entries.hasNext
      def next = {
        val e = entries.next
        val k = e.getKey
        val (cnt, time) = e.getValue
        (k, cnt.get, time.get)
      }
    }
  }

  private def mkTbl: Option[Table] = {
    if (map.size == 0) {
      None
    } else {
      val headers = Array("method", "#call", "time")
      val rows = tblIterator.map{ case (a,b,c) => Seq(a,b.toString,c.toString) }.toIterable
      Some(new Table("Method calls", headers, rows))
    }
  }
  
  private def mkComments: Option[Item] = {
    if (comments.size == 0) {
      None
    } else {
      val lst = new List("Comments")
      val it = comments.iterator
      while(it.hasNext) {
        lst.add(new Text("", it.next))
      }
      Some(lst)
    }
  }

  def report: Item = {
    val lst = new List("Statistics")
    for (t <- mkTbl) lst.add(t)
    for (c <- mkComments) lst.add(c)
    lst
  }

}

object Stats extends Stats {
}
