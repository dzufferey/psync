package psync.logic.quantifiers

import psync.formula._
import psync.formula.FormulaUtils._
import psync.logic._

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import dzufferey.utils.Namer
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ListBuffer

/** Tactics guide the quantifier instantiation. */
trait Tactic {

  def init(cc: CongruenceClosure): Unit

  def clear: Unit

  def hasNext: Boolean

  def next: Formula

  /* add the result from instantiating with `next` back into the tactic */
  def generatorResult(f: Iterable[Formula]): Unit

}

abstract class TacticCommon(depth: Formula => Int) extends Tactic {
  
  protected var cc: CongruenceClosure = null
  protected var queue = PriorityQueue[(Int,Formula)]()
  protected var done = MSet[Formula]()
  protected var currentDepth = 0
  
  protected def isDone(t: Formula) = done(t) || done(cc.repr(t))

  protected def enqueue(d: Int, t: Formula): Unit = {
    if (d < depth(t) && !isDone(t)) {
      Logger("Tactic", Debug, "depth "+d+": " + t)
      queue.enqueue( -d -> t )
    }/* else {
      Logger("Tactic", Debug, "rejecting depth "+d+": " + t)
    }*/
  }

  def clear: Unit = {
    queue.clear
    done.clear
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

  def init(_cc: CongruenceClosure): Unit = {
    clear
    cc = _cc
    for (gt <- cc.groundTerms) enqueue(0, gt)
  }

  def generatorResult(fs: Iterable[Formula]): Unit = {
    val newDepth = currentDepth + 1
    fs.foreach( f => {
      if (newDepth < depth(f)) {
        val ts = FormulaUtils.collectGroundTerms(f)
        val ts2 = ts.filter(t => !cc.contains(t))
        ts2.foreach(enqueue(newDepth, _))
      }
    })
    fs.foreach(cc.addConstraints)
  }

}

class Eager(depth: Map[Type,Int]) extends TacticCommon((t: Formula) => depth(t.tpe)) {

  override def toString = "Eager(" + depth + ")"

  def this(depth: Option[Int]) = this(Map[Type,Int]().withDefaultValue(depth.getOrElse(1000000)))

  def this(depth: Int) = this(Map[Type,Int]().withDefaultValue(depth))

}

object ByName {
  def apply(depth: Map[String,Int], term: Formula): Int = {
    // depth.getOrElse(Namer.getPrefixAndVersion(t.name)._1, 0)
    ???
  }
}

// depth for terms with a given name (prefix)
class ByName(depth: Map[String,Int]) extends TacticCommon((t: Formula) => ByName(depth, t) ) {

  override def toString = "ByName(" + depth + ")"

}

/* TODO depth for terms under given function symbols
class UnderSymbol(depth: Map[Symbol,Int]) extends Tactic {

  override def toString = "UnderSymbol(" + depth + ")"

  ...

}
*/

class Sequence(ts: Tactic*) extends Tactic {

  protected var index = 0
  protected var cc: CongruenceClosure = null

  def init(_cc: CongruenceClosure): Unit = {
    clear
    cc = _cc
    ts(index).init(cc)
    Logger("Sequence", Debug, "first tactic: " + ts(index))
  }

  def clear: Unit = {
    index = 0
    ts.foreach(_.clear)
  }

  protected def nextGen: Unit = {
    if (index < ts.length - 1) {
      index += 1
      Logger("Sequence", Debug, "moving to next tactic: " + ts(index))
      ts(index).init(cc)
    }
  }

  def hasNext = {
    if (ts(index).hasNext) {
      true
    } else {
      nextGen
      ts(index).hasNext
    }
  }

  def next = {
    ts(index).next
  }

  def generatorResult(f: Iterable[Formula]): Unit = {
    ts(index).generatorResult(f)
  }

}
