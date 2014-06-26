package round

import Algorithm._
import runtime.Group

import io.netty.buffer.ByteBuf
import io.netty.buffer.PooledByteBufAllocator

import scala.pickling._
import binary._

// http://stackoverflow.com/questions/18725699/scala-pickling-and-type-parameters

abstract class Round[A: SPickler: Unpickler: FastTypeTag] {

  def send(): Set[(A, ProcessID)]

  def update(mailbox: Set[(A, ProcessID)]): Unit

  protected def broadcast(msg: A): Set[(A, ProcessID)] = {
    group.replicas.foldLeft(Set.empty[(A,ProcessID)])( (acc, r) => acc + (msg -> r.id))
  }

  //////////////////
  // util methods //
  //////////////////

  private var group: Group = null
  def setGroup(g: Group) { group = g }

  //private val maxSize = 4096// 65536-1
  private val allocator = new PooledByteBufAllocator(true)
  protected final def getBuffer: ByteBuf = {
    allocator.buffer()
  }

  private final def serialize(payload: A, out: ByteBuf, withLength: Boolean = true, offset: Int = 8): Int = {
    if (offset > 0) out.writerIndex(out.writerIndex() + offset)
    val bytes0 = payload.pickle.value
    val length = bytes0.length
    if (withLength) {
      out.writeInt(length.toShort)
      out.writeBytes(bytes0)
      length + 4
    } else {
      out.writeBytes(bytes0)
      length
    }
  }

  private final def deserialize(in: ByteBuf, withLength: Boolean = true, offset: Int = 8): A = {
    if (offset > 0) in.readerIndex(in.readerIndex() + offset)
    val length = if (withLength) in.readInt() else in.readableBytes()
    val bytes = Array.ofDim[Byte](length)
    in.readBytes(bytes)
    BinaryPickle(bytes).unpickle[A]
  }

  final def packSend: Set[(ByteBuf, ProcessID)] = {
    send().map{ case (value, dst) =>
      val buf = getBuffer
      serialize(value, buf)
      (buf, dst)
    }
  }

  final def unpackUpdate(msg: Set[(ByteBuf,ProcessID)]) = {
    def decode(p: (ByteBuf,ProcessID)): (A, ProcessID) = {
      val p1 = p._1
      val a = deserialize(p1)
      p1.release
      (a, p._2)
    }
    val decoded = msg.map(decode)
    update(decoded)
  }

}



