package example

import java.util.concurrent.locks.ReentrantLock
import psync.runtime.Instance

/** some utils to easily store and access previous decision */
trait DecisionLog[T] {

  protected var nDecisions = 1000
  protected var decisionLocks = Array.ofDim[ReentrantLock](nDecisions)
  protected var decisions = Array.ofDim[(Short, T)](nDecisions)
  for (i <- 0 until nDecisions) decisionLocks(i) = new ReentrantLock()

  def setLogSize(n: Int) = {
    if (n != nDecisions) {
      nDecisions = n
      decisionLocks = Array.ofDim[ReentrantLock](n)
      decisions = Array.ofDim[(Short, T)](n)
      for (i <- 0 until n) decisionLocks(i) = new ReentrantLock()
    }
  }

  protected def decIdx(i: Int) = {
    val idx = i % nDecisions
    if (idx < 0) idx + nDecisions else idx
  }

  protected def pushDecision(inst: Short, dec: T) = {
    val prev = decisions(decIdx(inst))
    decisions(decIdx(inst)) = (inst -> dec)
    prev == null || Instance.lt(prev._1, inst)
  }

  protected def getDec(i: Short): Option[T] = {
    val candidate = decisions(decIdx(i))
    if (candidate == null || candidate._1 != i) None
    else Some(candidate._2)
  }

  protected def getLock(i: Int) = {
    decisionLocks(decIdx(i))
  }

}

