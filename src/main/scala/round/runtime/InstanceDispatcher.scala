package round.runtime

import round.predicate.Predicate
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import io.netty.channel._
import io.netty.channel.socket._

import java.util.concurrent.locks.ReentrantLock


//a dispatcher that scales better than the pipeline
class InstanceDispatcher(
    options: Map[String, String] = Map.empty
  ) extends SimpleChannelInboundHandler[DatagramPacket](false)
{

  private val exp = {
    try {
      options.getOrElse("dispatcher", "7").toInt
    } catch { case e: Exception =>
      Logger("Predicate", Warning, "dispatcher unspecified or wrong format, using 7")
      7
    }
  }
  private val shift = {
    val res = 32 - exp
    assert(res >= 0)
    res
  }
  private val n = {
    val res = 1 << exp // 2^exp
    assert(res >= 0)
    res
  }

  private val locks = Array.ofDim[ReentrantLock](n)
  private val instances = Array.ofDim[List[(Int, Predicate)]](n)

  for ( i <- 0 until n ) {
    locks(i) = new ReentrantLock
    instances(i) = Nil
  }

  private def index(inst: Int): Int = {
    inst << shift >>> shift
  }

  def add(inst: Int, handler: Predicate ) {
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
    var oldLst: List[(Int,Predicate)] = Nil
    l.lock()
    try {
      oldLst = instances(i)
      val lst2 = oldLst.filter( p => p._1 != inst )
      instances(i) = lst2
    } finally {
      l.unlock()
    }
    if (oldLst forall (_._1 != inst)) {
      sys.error("dispatcher.remove: instance not found")
    }
  }

  private def findInstance(inst: Int): Option[Predicate] = {
    val i = index(inst)
    instances(i).find( p => p._1 == inst).map(_._2)
  }

  //in Netty version 5.0 will be called: channelRead0 will be messageReceived
  override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
    val tag = Message.getTag(pkt.content)
    findInstance(tag.instanceNbr) match {
      case Some(inst) => inst.messageReceived(ctx, pkt)
      case None => ctx.fireChannelRead(pkt)
    }
  }

}
