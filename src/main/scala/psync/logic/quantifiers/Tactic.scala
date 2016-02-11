package psync.logic.quantifiers

import psync.formula._
import psync.formula.FormulaUtils._
import psync.logic._

import dzufferey.utils.{Misc, Namer}
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ListBuffer

trait Tactic {

  def init(depth: Int, cc: CongruenceClosure): Unit

  def clear: Unit

  def hasNext: Boolean

  def next: Formula

  def generatorResult(f: Iterable[Formula]): Unit

  def result: Iterable[Formula]

  def leftOut: Iterable[Formula]

}

abstract class TacticCommon extends Tactic {
  
  protected var depth = 0
  protected var cc: CongruenceClosure = null
  protected var queue = PriorityQueue[(Int,Formula)]()
  protected var done = MSet[Formula]()
  protected var currentDepth = 0
  protected val buffer = ListBuffer[Formula]()
  
  protected def isDone(t: Formula) = done(t) || done(cc.repr(t))

  protected def enqueue(d: Int, t: Formula) {
    if (d <= depth && !isDone(t)) {
      Logger("Tactic", Debug, "depth "+d+": " + t)
      queue.enqueue( -d -> t )
    }
  }

  def clear {
    queue.clear
    done.clear
    buffer.clear
    depth = 0
    currentDepth = 0
    cc = null
  }
  
  def hasNext: Boolean = queue.headOption match {
    case Some((lvl,term)) =>
      if (isDone(term)) {
        queue.dequeue
        hasNext
      } else {
        true
      }
    case None => false
  }

  def next: Formula = {
    val (lvl, term) = queue.dequeue
    currentDepth = -lvl
    done += term
    done += cc.repr(term)
    term
  }

  def init(_depth: Int, _cc: CongruenceClosure) {
    clear
    depth = _depth
    cc = _cc
    for (gt <- cc.groundTerms) enqueue(0, gt)
  }
  
  def result: Iterable[Formula] = buffer.toList

}

class Eager extends TacticCommon {

  def generatorResult(fs: Iterable[Formula]) {
    buffer.appendAll(fs)
    val newDepth = currentDepth + 1
    if (newDepth < depth) {
      for (f <- fs;
           t <- FormulaUtils.collectGroundTerms(f) if !cc.contains(t) )
      {
        enqueue(newDepth, t)
      }
    }
    fs.foreach(cc.addConstraints)
  }

  def leftOut: Iterable[Formula] = Nil

}

class Guided extends TacticCommon {

  protected var currentTerm: Formula = null
  protected val keptBack = MMap[Formula,List[Formula]]()

  override def clear {
    super.clear
    currentTerm = null
    keptBack.clear
  }

  override def next: Formula = {
    val term = super.next
    currentTerm = term
    term
  }

  def generatorResult(fs: Iterable[Formula]) {
    val toAdd = MSet[Formula]()
    for (f <- fs) {
      val gts = FormulaUtils.collectGroundTerms(f)
      val (_old,_new) = gts.partition(cc.contains)
      if (_new.isEmpty ||
          _old.exists(FormulaUtils.isDescendent(_, currentTerm))) {
        //println("adding: " + f + ", " + _new + ", " + _old)
        toAdd += f
      } else {
        //println("keeping: " + f + ", " + _new + ", " + _old)
        for (n <- _new) {
          val ks = keptBack.getOrElse(n, Nil)
          keptBack += (n -> (f :: ks))
        }
      }
    }
    while (!toAdd.isEmpty) {
      //add to cc, buffer, and queue
      currentDepth += 1
      buffer.appendAll(fs)
      for (f <- toAdd;
           t <- FormulaUtils.collectGroundTerms(f) if !cc.contains(t) )
      {
        enqueue(currentDepth, t)
      }
      toAdd.foreach(cc.addConstraints)
      toAdd.clear()
      // check if we should pull some new keptBack elements
      for (k <- keptBack.keys if cc.contains(k)) {
        val ts = keptBack(k)
        toAdd ++= ts
        keptBack += (k -> Nil)
      }
    }
  }

  def leftOut: Iterable[Formula] = keptBack.values.flatten.toSet

}

class Sequence(t1: Tactic, t2: Tactic) extends Tactic {

  protected var first = true
  protected var d1 = 0
  protected var d2 = 0
  protected var cc: CongruenceClosure = null

  def init(depth: Int, _cc: CongruenceClosure) {
    clear
    cc = _cc
    d1 = depth / 2 + depth % 2
    d2 = depth / 2
    t1.init(d1, cc)
  }

  def clear {
    first = true
    d1 = 0
    d2 = 0
    t1.clear
    t2.clear
  }

  def hasNext = {
    if (first) {
      if (t1.hasNext) {
        true
      } else {
        first = false
        t2.init(d2, cc)
        t2.hasNext
      }
    } else {
      t2.hasNext
    }
  }

  def next = {
    if (first) t1.next
    else t2.next
  }

  def generatorResult(f: Iterable[Formula]) {
    if (first) t1.generatorResult(f)
    else t2.generatorResult(f)
  }

  def result = t1.result ++ t2.result

  def leftOut = t1.leftOut ++ t2.leftOut

}
