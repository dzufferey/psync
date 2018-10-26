package psync.utils.serialization

import io.netty.buffer.ByteBuf
import com.esotericsoftware.kryo.io.Input
import com.esotericsoftware.kryo.KryoException
import java.nio.charset.StandardCharsets

class KryoByteBufInput(protected var bbuffer: ByteBuf) extends Input {
  def setBuffer(b: ByteBuf) = bbuffer = b
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
  // TODO variable encoding
  override def readInt(): Int = bbuffer.readInt
  override def readInt(optimizePositive: Boolean): Int = bbuffer.readInt
  override def readVarInt (optimizePositive: Boolean): Int = bbuffer.readInt
  override def canReadInt(): Boolean = available >= 4
  override def canReadLong(): Boolean = available >= 8
  override def readString() = bbuffer.toString(StandardCharsets.UTF_8)
  override def readStringBuilder () = new java.lang.StringBuilder(readString)
  override def readFloat() = bbuffer.readFloat
  override def readFloat(precision: Float, optimizePositive: Boolean) = bbuffer.readFloat
  override def readShort() = bbuffer.readShort
  override def readShortUnsigned() = bbuffer.readUnsignedShort
  override def readLong() = bbuffer.readLong
  override def readLong(optimizePositive: Boolean) = bbuffer.readLong
  override def readVarLong(optimizePositive: Boolean) = bbuffer.readLong 
  override def readBoolean() = bbuffer.readBoolean
  override def readChar() = bbuffer.readChar
  override def readDouble() = bbuffer.readDouble
  override def readDouble(precision: Double, optimizePositive: Boolean) = bbuffer.readDouble
}
