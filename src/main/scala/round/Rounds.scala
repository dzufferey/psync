package round

import Algorithm._

import io.netty.buffer.ByteBuf

import scala.pickling._
import binary._

abstract class Round[A: SPickler: Unpickler: FastTypeTag] {

  def send(): Set[(A, ProcessID)]

  def update(mailbox: Set[(A, ProcessID)]): Unit

  final protected def broadcast(msg: A): Set[(A, ProcessID)] = {
    sys.error("not yet implemented")
  }

  //////////////////
  // util methods //
  //////////////////

  private def getBuffer(): ByteBuf = {
    sys.error("have macro fill the gap ...")
    //use the getBuffer from the Algorithm context
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



