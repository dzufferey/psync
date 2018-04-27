package psync.macros

import psync.formula._
import psync.logic._
import dzufferey.utils.Namer

import com.esotericsoftware.kryo.KryoException
import java.nio.charset.StandardCharsets

class ByteBufInputKryoInput(protected var bbuffer: io.netty.buffer.ByteBuf) extends com.esotericsoftware.kryo.io.Input {
  def setBuffer(b: io.netty.buffer.ByteBuf) = bbuffer = b
  override def setBuffer(bytes: Array[Byte], offset: Int, count: Int) = ???
  override def getBuffer(): Array[Byte] = ???
  override def getInputStream(): java.io.InputStream = ???
  override def setInputStream(inputStream: java.io.InputStream) = ???
  override def total(): Long = ???
  override def setTotal(t: Long) = ???
  override def position = bbuffer.readerIndex
  override def setPosition(index: Int) = bbuffer.readerIndex(index)
  override def limit = bbuffer.writerIndex
  override def rewind { bbuffer.clear }
  override def skip(count: Int) { bbuffer.skipBytes(count) }
  override protected def fill(buffer: Array[Byte], offset: Int, count: Int): Int = {
    readBytes(buffer, offset, count)
    count
  }
  override protected def require(required: Int): Int = {
    val count = bbuffer.readableBytes
    if (count < required) {
        throw new KryoException("not enough bytes to read: " + count + " < " + required + ".")
    } else {
        count
    }
  }
  override def eof() = bbuffer.readableBytes <= 0
  override def available(): Int = bbuffer.readableBytes
  override def read(): Int = bbuffer.readByte & 0xFF
  override def read(bytes: Array[Byte]): Int = read(bytes, 0, bytes.length)
  override def read(bytes: Array[Byte], offset: Int, count: Int): Int = {
    bbuffer.readBytes(bytes, offset, count)
    count
  }
  override def skip(count: Long): Long = {
    assert(count < scala.Int.MaxValue)
    bbuffer.skipBytes(count.toInt);
    count
  }
  override def close() {}
  override def readByte(): Byte = bbuffer.readByte
  override def readByteUnsigned() = bbuffer.readUnsignedByte
  override def readBytes(length: Int) = {
    val b = Array.ofDim[Byte](length)
    bbuffer.readBytes(b, 0, length)
    b
  }
  override def readBytes (bytes: Array[Byte]) = {
    bbuffer.readBytes(bytes)
    bytes.length
  }
  override def readBytes(bytes: Array[Byte], offset: Int, count: Int) {
    bbuffer.readBytes(bytes, offset, count)
  }
  override def readInt(): Int = bbuffer.readInt
  override def readInt(optimizePositive: Boolean): Int = bbuffer.readInt // TODO variable encoding
  override def readVarInt (optimizePositive: Boolean): Int = bbuffer.readInt // TODO variable encoding
  override def canReadInt(): Boolean = available >= 4
  override def canReadLong(): Boolean = available >= 8
  override def readString() = bbuffer.toString(StandardCharsets.UTF_8)
  override def readStringBuilder () = new java.lang.StringBuilder(readString)
  override def readFloat() = bbuffer.readFloat
  override def readFloat(precision: Float, optimizePositive: Boolean) = bbuffer.readFloat // TODO
  override def readShort() = bbuffer.readShort
  override def readShortUnsigned() = bbuffer.readUnsignedShort
  override def readLong() = bbuffer.readLong
  override def readLong(optimizePositive: Boolean) = bbuffer.readLong // TODO
  override def readVarLong(optimizePositive: Boolean) = bbuffer.readLong // TODO
  override def readBoolean() = bbuffer.readBoolean
  override def readChar() = bbuffer.readChar
  override def readDouble() = bbuffer.readDouble
  override def readDouble(precision: Double, optimizePositive: Boolean) = bbuffer.readDouble // TODO
}

class ByteBufInputKryoOutput(protected var bbuffer: io.netty.buffer.ByteBuf) extends com.esotericsoftware.kryo.io.Output {
  def setBuffer(b: io.netty.buffer.ByteBuf) = bbuffer = b
  override def setOutputStream(outputStream: java.io.OutputStream) = ???
  override def setBuffer(buffer: Array[Byte]) = ???
  override def setBuffer(buffer: Array[Byte], maxBufferSize: Int) = ???
  override def getBuffer() = ???
  override def toBytes() = bbuffer.array()
  override def position() = bbuffer.writerIndex
  override def setPosition(position: Int) = bbuffer.writerIndex(position)
  def skip(count: Int) {  bbuffer.writerIndex(bbuffer.writerIndex + count) }
  override def total() = position()
  override def clear() = bbuffer.clear
  override protected def require(required: Int) = {
    bbuffer.ensureWritable(required, false) match {
      case 0 => false
      case 1 => throw new KryoException("not enough capacity")
      case 2 | 3 => true
      case _ => throw new KryoException("???")
    }
  }
  override def flush() {}
  override def close() {} 
  override def write(value: Int) = bbuffer.writeByte(value.toByte)
  override def write(bytes: Array[Byte]) = bbuffer.writeBytes(bytes)
  override def write(bytes: Array[Byte], offset: Int, length: Int) = bbuffer.writeBytes(bytes, offset, length)
  override def writeByte(value: Byte) = bbuffer.writeByte(value)
  override def writeByte(value: Int) = bbuffer.writeByte(value.toByte)
  override def writeBytes(bytes: Array[Byte]) = bbuffer.writeBytes(bytes)
  override def writeBytes(bytes: Array[Byte], offset: Int, count: Int) = bbuffer.writeBytes(bytes, offset, count)
  override def writeInt(value: Int) = bbuffer.writeInt(value)
  override def writeInt(value: Int, optimizePositive: Boolean) = { bbuffer.writeInt(value); 4 } // TODO
  override def writeVarInt(value: Int, optimizePositive: Boolean) = { bbuffer.writeInt(value); 4 } // TODO
  override def writeString(value: String) = io.netty.buffer.ByteBufUtil.writeUtf8(bbuffer, value)
  override def writeString(value: CharSequence) = io.netty.buffer.ByteBufUtil.writeUtf8(bbuffer, value)
  override def writeAscii(value: String) =  io.netty.buffer.ByteBufUtil.writeAscii(bbuffer, value)
  override def writeFloat(value: Float) = bbuffer.writeFloat(value)
  override def writeFloat(value: Float, precision: Float, optimizePositive: Boolean) = { bbuffer.writeFloat(value); 4 } // TODO
  override def writeShort(value: Int) = bbuffer.writeShort(value.toShort)
  override def writeLong(value: Long) = bbuffer.writeLong(value)
  override def writeLong(value: Long, optimizePositive: Boolean) = { bbuffer.writeLong(value); 8 } // TODO
  override def writeVarLong(value: Long, optimizePositive: Boolean) = { bbuffer.writeLong(value); 8 } // TODO
  override def writeBoolean(value: Boolean) = bbuffer.writeBoolean(value)
  override def writeChar(value: Char) = bbuffer.writeChar(value)
  override def writeDouble(value: Double) = bbuffer.writeDouble(value)
  override def writeDouble(value: Double, precision: Double, optimizePositive: Boolean) = { bbuffer.writeDouble(value); 8 } // TODO
}


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


object KryoSerializer {

  import com.twitter.chill._
  private val inst = new ScalaKryoInstantiator

  def serializer = {
    val kryo = inst.newKryo
    kryo
  }

}


trait Serialization {
  self: Impl =>
  import c.universe._
  
  def kryoIO(tpt: Tree) = List(
      q"private val serializer = _root_.psync.macros.KryoSerializer.serializer",
      q"private val kryoOut = new _root_.psync.macros.ByteBufInputKryoOutput(null)",
      q"private val kryoIn = new _root_.psync.macros.ByteBufInputKryoInput(null)",
      q"serializer.register(classOf[$tpt])",
      q"""protected def serialize(payload: $tpt, out: _root_.io.netty.buffer.ByteBuf): Unit = {
        kryoOut.setBuffer(out)
        serializer.writeObject(kryoOut, payload)
        kryoOut.setBuffer(null: _root_.io.netty.buffer.ByteBuf)
      }""",
      q"""protected def deserialize(in: _root_.io.netty.buffer.ByteBuf): $tpt = {
        kryoIn.setBuffer(in)
        val result = serializer.readObject(kryoIn, classOf[$tpt])
        kryoIn.setBuffer(null: _root_.io.netty.buffer.ByteBuf)
        result
      }"""
  )

  def picklingIO(tpt: Tree) = List(
      q"""protected def serialize(payload: $tpt, out: _root_.io.netty.buffer.ByteBuf): Unit = {
        import scala.pickling._
        import scala.pickling.Defaults._
        import binary._
        import static._
        payload.pickleTo(new _root_.psync.macros.ByteBufOutput(out))
      }""",
      q"""protected def deserialize(in: _root_.io.netty.buffer.ByteBuf): $tpt = {
        import scala.pickling._
        import scala.pickling.Defaults._
        import binary._
        import static._
        val pickle = BinaryPickle(new _root_.psync.macros.ByteBufInput(in))
        pickle.unpickle[$tpt]
      }"""
    )

  def serializationMethods(tpt: Tree): List[Tree] = {
    //println("using pickling on " + showRaw(tpt))
    picklingIO(tpt)
    //kryoIO(tpt)
  }

}
