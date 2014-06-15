package round.runtime

import java.net.DatagramPacket
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import io.netty.buffer.ByteBuf

import scala.pickling._
import binary._
// http://stackoverflow.com/questions/18725699/scala-pickling-and-type-parameters

//TODO extends ByteBufHolder ?
class Message[A: SPickler: FastTypeTag](
    val payload: A,
    val senderId: Short,
    val receiverId: Short,
    val sequenceId: Int ) {

  /** Serialize data and put them in a ByteBuffer */
  def toByteBuffer(direct: Boolean = true) = {
    val bytes0 = payload.pickle.value
    val length = bytes0.length
    val size = length + 2 + 4 + 4
    val buffer =
      if (direct) ByteBuffer.allocateDirect(size)
      else ByteBuffer.allocate(size)
    buffer.putInt(sequenceId)
    buffer.putShort(senderId)
    buffer.putInt(length.toShort)
    buffer.put(bytes0)
    buffer
  }

  /** Serialize to a netty ByteBuf, returns the number of bytes written */
  def toByteBuf(out: ByteBuf) = {
    val bytes0 = payload.pickle.value
    val length = bytes0.length
    out.writeInt(sequenceId)
    out.writeShort(senderId)
    out.writeInt(length.toShort)
    out.writeBytes(bytes0)
    length + 2 + 4 + 4
  }


}


object Message {

  /** Deserialize the data contained in a ByteBuffer */
  def fromByteBuffer[A: SPickler: Unpickler: FastTypeTag](self: Short, buffer: ByteBuffer): Message[A] = {
    val sequenceId = buffer.getInt()
    val senderId = buffer.getShort()
    val length = buffer.getInt()
    val bytes = Array.ofDim[Byte](length)
    buffer.get(bytes)
    val payload = BinaryPickle(bytes).unpickle[A]
    new Message(payload, senderId, self, sequenceId)
  }
  
  /** Deserialize the data contained in a ByteBuf */
  def fromByteBuf[A: SPickler: Unpickler: FastTypeTag](self: Short, buffer: ByteBuf): Message[A] = {
    val sequenceId = buffer.readInt()
    val senderId = buffer.readShort()
    val length = buffer.readInt()
    val bytes = Array.ofDim[Byte](length)
    buffer.readBytes(bytes)
    val payload = BinaryPickle(bytes).unpickle[A]
    new Message(payload, senderId, self, sequenceId)
  }

}
