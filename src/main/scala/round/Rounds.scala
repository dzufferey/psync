package round

import io.netty.buffer.ByteBuf

import scala.pickling._
import binary._

//TODO can we take that out of the Algorithm ?

trait Rounds[IO] {
  self: Algorithm[IO] =>

  abstract class Round[A: SPickler: Unpickler: FastTypeTag] {

    def send(): Set[(A, Process)]

    def update(mailbox: Set[(A, Process)]): Unit

    final protected def broadcast(msg: A): Set[(A, Process)] = {
      sys.error("not yet implemented")
    }

    //////////////////
    // util methods //
    //////////////////

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

    private final def deserialize(in: ByteBuf, withLength: Boolean = false, offset: Int = 0): A = {
      if (offset > 0) in.readerIndex(in.readerIndex() + offset)
      val length = if (withLength) in.readInt() else in.readableBytes()
      val bytes = Array.ofDim[Byte](length)
      in.readBytes(bytes)
      BinaryPickle(bytes).unpickle[A]
    }

    final def packSend = {
      send().map{ case (value, dst) => (serialize(value, getBuffer), dst) }
    }

    final def unpackUpdate(msg: Set[(ByteBuf,Process)]) = {
      //TODO set the HO
      def decode(p: (ByteBuf,Process)): (A, Process) = {
        val p1 = p._1
        val a = deserialize(p1)
        p1.release
        (a, p._2)
      }
      val decoded = msg.map(decode)
      update(decoded)
    }

  }

}

