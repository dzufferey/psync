package round.runtime

import java.net.DatagramPacket
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import io.netty.buffer.ByteBuf

import scala.pickling._
import binary._
// http://stackoverflow.com/questions/18725699/scala-pickling-and-type-parameters

object Message {

  /** Serialize to a netty ByteBuf, returns the number of bytes written */
  def toByteBuffer[A: SPickler: FastTypeTag](
        payload: A,
        sequenceId: Int,
        senderId: Short,
        out: ByteBuf
      ): Int = {
    val bytes0 = payload.pickle.value
    val length = bytes0.length
    out.writeInt(sequenceId)
    out.writeShort(senderId)
    out.writeInt(length.toShort)
    out.writeBytes(bytes0)
    length + 2 + 4 + 4
  }

  /** Serialize data and put them in a ByteBuffer */
  def toByteBuffer[A: SPickler: FastTypeTag](
        payload: A,
        sequenceId: Int,
        senderId: Short,
        direct: Boolean = true
      ): ByteBuffer = {
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

  /** Serialize data and put them in a datagram packet.
   *  payload is the userdefined type of message
   *  sequenceId is an integer use by our framework
   *  senderId is the ID of the sender
   */
  def toPacket[A: SPickler: FastTypeTag](
        payload: A,
        sequenceId: Int,
        senderId: Short,
        recipient: InetSocketAddress
      ): DatagramPacket = {
    val buffer = toByteBuffer(payload, senderId, senderId, false)
    val bytes = buffer.array
    new DatagramPacket(bytes, bytes.length, recipient)
  }

  /** Deserialize the data contained in a ByteBuffer */
  def fromByteBuffer[A: Unpickler: FastTypeTag](buffer: ByteBuffer): (Int, Short, A) = {
    val sequenceId = buffer.getInt()
    val senderId = buffer.getShort()
    val length = buffer.getInt()
    val bytes = Array.ofDim[Byte](length)
    buffer.get(bytes)
    val payload = BinaryPickle(bytes).unpickle[A]
    (sequenceId, senderId, payload)
  }
  
  /** Deserialize the data contained in a ByteBuf */
  def fromByteBuf[A: Unpickler: FastTypeTag](buffer: ByteBuf): (Int, Short, A) = {
    fromByteBuffer(buffer.nioBuffer())
  }

  /** Deserialize the data contained in a datagram packet */
  def fromPacket[A: Unpickler: FastTypeTag](pkt: DatagramPacket): (Int, Short, A) = {
    val buffer = ByteBuffer.wrap(pkt.getData)
    fromByteBuffer(buffer)
  }

}
