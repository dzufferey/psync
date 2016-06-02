package psync.macros

import psync.formula._
import psync.logic._
import dzufferey.utils.Namer

class ByteBufInput(buffer: io.netty.buffer.ByteBuf) extends scala.pickling.binary.BinaryInput {
  import io.netty.buffer.ByteBuf
  def getByte() = buffer.readByte
  def getChar() = buffer.readInt.toChar
  def getShort() = buffer.readShort
  def getInt() = buffer.readInt
  def getLong() = buffer.readLong
  def getFloat() = buffer.readFloat
  def getDouble() = buffer.readDouble
  def getBytes(target: Array[Byte], len: Int): Unit = {
    buffer.readBytes(target, 0, len)
  }
}

class ByteBufOutput(buffer: io.netty.buffer.ByteBuf) extends scala.pickling.binary.BinaryOutput {
  import io.netty.buffer.ByteBuf
  def result: Array[Byte] = null
  def ensureCapacity(capacity: Int) {
    if (capacity > 0) {
      buffer.ensureWritable(capacity)
      if (buffer.writableBytes < capacity) {
        throw new java.nio.BufferOverflowException()
      }
    }
  }
  def putByte(value: Byte) = buffer.writeByte(value)
  def putChar(value: Char) = buffer.writeInt(value.toInt)
  def putShort(value: Short) = buffer.writeShort(value)
  def putInt(value: Int) = buffer.writeInt(value)
  def putLong(value: Long) = buffer.writeLong(value)
  def putFloat(value: Float) = buffer.writeFloat(value)
  def putDouble(value: Double) = buffer.writeDouble(value)
  def putBytes(value: Array[Byte], len: Int) = buffer.writeBytes(value, 0, len)
}

trait Serialization {
  self: Impl =>
  import c.universe._

  def picklingIO(tpt: Tree) = List(
      q"""protected def serialize(payload: $tpt, out: _root_.io.netty.buffer.ByteBuf): Unit = {
        import scala.pickling._
        import scala.pickling.Defaults._
        import binary._
        out.writerIndex(out.writerIndex() + _root_.psync.runtime.Tag.size)
        payload.pickleTo(new _root_.psync.macros.ByteBufOutput(out))
      }""",
      q"""protected def deserialize(in: _root_.io.netty.buffer.ByteBuf): $tpt = {
        import scala.pickling._
        import scala.pickling.Defaults._
        import binary._
        in.readerIndex(in.readerIndex() + _root_.psync.runtime.Tag.size)
        val pickle = BinaryPickle(new _root_.psync.macros.ByteBufInput(in))
        pickle.unpickle[$tpt]
      }"""
    )

  def serializationMethods(tpt: Tree): List[Tree] = {
    //println("using pickling on " + showRaw(tpt))
    picklingIO(tpt)
  }

}
