package psync.utils.serialization

import io.netty.buffer.{ByteBuf, ByteBufUtil}
import com.esotericsoftware.kryo.io.Output
import com.esotericsoftware.kryo.KryoException

class KryoByteBufOutput(protected var bbuffer: ByteBuf) extends Output {
  def setBuffer(b: ByteBuf) = bbuffer = b
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
  // TODO variable encoding
  override def write(value: Int) = bbuffer.writeByte(value.toByte)
  override def write(bytes: Array[Byte]) = bbuffer.writeBytes(bytes)
  override def write(bytes: Array[Byte], offset: Int, length: Int) = bbuffer.writeBytes(bytes, offset, length)
  override def writeByte(value: Byte) = bbuffer.writeByte(value)
  override def writeByte(value: Int) = bbuffer.writeByte(value.toByte)
  override def writeBytes(bytes: Array[Byte]) = bbuffer.writeBytes(bytes)
  override def writeBytes(bytes: Array[Byte], offset: Int, count: Int) = bbuffer.writeBytes(bytes, offset, count)
  override def writeInt(value: Int) = bbuffer.writeInt(value)
  override def writeInt(value: Int, optimizePositive: Boolean) = { bbuffer.writeInt(value); 4 } // linter:ignore InvariantReturn
  override def writeVarInt(value: Int, optimizePositive: Boolean) = { bbuffer.writeInt(value); 4 } // linter:ignore InvariantReturn
  override def writeString(value: String) = ByteBufUtil.writeUtf8(bbuffer, value)
  override def writeString(value: CharSequence) = ByteBufUtil.writeUtf8(bbuffer, value)
  override def writeAscii(value: String) =  ByteBufUtil.writeAscii(bbuffer, value)
  override def writeFloat(value: Float) = bbuffer.writeFloat(value)
  override def writeFloat(value: Float, precision: Float, optimizePositive: Boolean) = { bbuffer.writeFloat(value); 4 } // linter:ignore InvariantReturn
  override def writeShort(value: Int) = bbuffer.writeShort(value.toShort)
  override def writeLong(value: Long) = bbuffer.writeLong(value)
  override def writeLong(value: Long, optimizePositive: Boolean) = { bbuffer.writeLong(value); 8 } // linter:ignore InvariantReturn
  override def writeVarLong(value: Long, optimizePositive: Boolean) = { bbuffer.writeLong(value); 8 } // linter:ignore InvariantReturn
  override def writeBoolean(value: Boolean) = bbuffer.writeBoolean(value)
  override def writeChar(value: Char) = bbuffer.writeChar(value)
  override def writeDouble(value: Double) = bbuffer.writeDouble(value)
  override def writeDouble(value: Double, precision: Double, optimizePositive: Boolean) = { bbuffer.writeDouble(value); 8 } // linter:ignore InvariantReturn
}

