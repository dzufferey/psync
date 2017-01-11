package psync.runtime

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import io.netty.channel.socket._
import java.util.concurrent.locks.ReentrantLock

/** a dispatcher that scales better than putting all the instance in the pipeline
 * @param dispatchExponent log₂ fan out of the dispatcher (recommended ~6)
 */
class Dispatcher(dispatchExponent: Int)
{

  private val exp = dispatchExponent
  assert(exp >= 0)

  private val mask = {
    var x = 0
    for (_ <- 0 until exp) {
      x = x << 1
      x |= 1
    }
    x
  }
  private val n = {
    val res = 1 << exp // 2^exp
    assert(res >= 0)
    res
  }

  private val locks = Array.ofDim[ReentrantLock](n)
  private val instances = Array.ofDim[List[(Int, InstHandler)]](n)

  for ( i <- 0 until n ) {
    locks(i) = new ReentrantLock
    instances(i) = Nil
  }

  private def index(inst: Int): Int = inst & mask

  def add(inst: Int, handler: InstHandler) {
    val i = index(inst)
    val l = locks(i)
    l.lock()
    try {
      val lst = instances(i)
      if (lst exists (_._1 == inst)) {
        sys.error("cannot run more than one instance with the same ID: " + inst)
      }
      val lst2 = (inst, handler) :: lst
      instances(i) = lst2
    } finally {
      l.unlock()
    }
  }
  
  def remove(inst: Int) {
    val i = index(inst)
    val l = locks(i)
    var oldLst: List[(Int,InstHandler)] = Nil
    l.lock()
    try {
      oldLst = instances(i) // linter:ignore VariableAssignedUnusedValue
      val lst2 = oldLst.filter( p => p._1 != inst )
      instances(i) = lst2
    } finally {
      l.unlock()
    }
    if (oldLst forall (_._1 != inst)) {
      Logger("InstanceDispatcher", Info, "dispatcher.remove: instance not found " + inst)
    }
  }

  def findInstance(inst: Int): Option[InstHandler] = {
    val i = index(inst)
    instances(i).find( p => p._1 == inst).map(_._2)
  }

  /** remove all the instances from the dispatch table */
  def clear {
    for ( i <- 0 until n ) {
      instances(i) = Nil
    }
  }

  def dispatch(pkt: DatagramPacket) = {
    val tag = Message.getTag(pkt.content)
    findInstance(tag.instanceNbr).exists( inst => {
      inst.newPacket(pkt)
      true
    })
  }

}
