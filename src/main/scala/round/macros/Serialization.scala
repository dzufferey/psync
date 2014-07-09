package round.macros

import round.formula._
import round.logic._
import round.utils.Namer

trait Serialization {
  self: Impl =>
  import c.universe._

  //$tpt -> protected def serialize(payload: $tpt, out: _root_.io.netty.buffer.ByteBuf, withLength: Boolean = true, offset: Int = 8): Int
  //$tpt -> protected def deserialize(in: _root_.io.netty.buffer.ByteBuf, withLength: Boolean = true, offset: Int = 8): $tpt
  
  
  def picklingIO(tpt: Tree) = List(
      q"""protected def serialize(payload: $tpt, out: _root_.io.netty.buffer.ByteBuf, withLength: Boolean = true, offset: Int = 8): Int = {
        import scala.pickling._
        import binary._
        if (offset > 0) out.writerIndex(out.writerIndex() + offset)
        val bytes0 = payload.pickle.value
        val length = bytes0.length
        if (withLength) {
          out.writeInt(length)
          out.writeBytes(bytes0)
          length + 4
        } else {
          out.writeBytes(bytes0)
          length
        }
      }""",
      q"""protected def deserialize(in: _root_.io.netty.buffer.ByteBuf, withLength: Boolean = true, offset: Int = 8): $tpt = {
        import scala.pickling._
        import binary._
        if (offset > 0) in.readerIndex(in.readerIndex() + offset)
        val length = if (withLength) in.readInt() else in.readableBytes()
        val bytes = Array.ofDim[Byte](length)
        in.readBytes(bytes)
        BinaryPickle(bytes).unpickle[$tpt]
      }"""
    )

  //TODO we first need to get rid of Message.scala
  def primitiveIO(tpt: Tree, length: Int, write: List[Tree], read: List[Tree]) = List(
      q"""protected def serialize(payload: $tpt, out: _root_.io.netty.buffer.ByteBuf, withLength: Boolean = true, offset: Int = 8): Int = {
        if (offset > 0) out.writerIndex(out.writerIndex() + offset)
        if (withLength) {
          out.writeInt($length.toShort)
          ..$write
          $length + 4
        } else {
          ..$write
          $length
        }
      }""",
      q"""protected def deserialize(in: _root_.io.netty.buffer.ByteBuf, withLength: Boolean = true, offset: Int = 8): $tpt = {
        if (offset > 0) in.readerIndex(in.readerIndex() + offset)
        val length = if (withLength) in.readInt() else in.readableBytes()
        ..$read
      }"""
  )

  def serializationMethods(tpt: Tree): List[Tree] = {
    picklingIO(tpt)
  }

}
