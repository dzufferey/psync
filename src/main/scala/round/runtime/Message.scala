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
    val tag: Tag) {


  def instance = tag.instanceNbr

  def round = tag.roundNbr

  /** Serialize data and put them in a ByteBuffer */
  def toByteBuffer(direct: Boolean = true) = {
    val bytes0 = payload.pickle.value
    val length = bytes0.length
    val size = length + 4 + tag.size
    val buffer =
      if (direct) ByteBuffer.allocateDirect(size)
      else ByteBuffer.allocate(size)
    buffer.putLong(tag.underlying)
    buffer.putInt(length.toShort)
    buffer.put(bytes0)
    buffer
  }

  /** Serialize to a netty ByteBuf, returns the number of bytes written */
  def toByteBuf(out: ByteBuf) = {
    val bytes0 = payload.pickle.value
    val length = bytes0.length
    out.writeLong(tag.underlying)
    out.writeInt(length.toShort)
    out.writeBytes(bytes0)
    length + 4 + tag.size
  }


}


object Message {

  /** Deserialize the data contained in a ByteBuffer */
  def fromByteBuffer[A: SPickler: Unpickler: FastTypeTag](
        sender: Short,
        receiver: Short,
        buffer: ByteBuffer
      ): Message[A] = {
    val tag = new Tag(buffer.getLong())
    val length = buffer.getInt()
    val bytes = Array.ofDim[Byte](length)
    buffer.get(bytes)
    val payload = BinaryPickle(bytes).unpickle[A]
    new Message(payload, sender, receiver, tag)
  }
  
  /** Deserialize the data contained in a ByteBuf */
  def fromByteBuf[A: SPickler: Unpickler: FastTypeTag](
        sender: Short,
        receiver: Short,
        buffer: ByteBuf
      ): Message[A] = {
    val tag = new Tag(buffer.readLong())
    val senderId = buffer.readShort()
    val length = buffer.readInt()
    val bytes = Array.ofDim[Byte](length)
    buffer.readBytes(bytes)
    val payload = BinaryPickle(bytes).unpickle[A]
    new Message(payload, sender, receiver, tag)
  }

}
