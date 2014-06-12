package round.runtime

import java.net.DatagramPacket
import java.net.InetSocketAddress
import java.nio.ByteBuffer

import scala.pickling._
import binary._
// http://stackoverflow.com/questions/18725699/scala-pickling-and-type-parameters

object Message {

  def toPacket[A: SPickler: FastTypeTag](
        payload: A,
        sequenceId: Int,
        senderId: Short,
        recipient: InetSocketAddress
      ): DatagramPacket = {
    val bytes0 = payload.pickle.value
    val length = bytes0.length
    val bytes = Array.ofDim[Byte](length + 2 + 4 + 4)
    val buffer = ByteBuffer.wrap(bytes)
    buffer.putInt(sequenceId)
    buffer.putShort(senderId)
    buffer.putInt(length.toShort)
    buffer.put(bytes0)
    new DatagramPacket(bytes, length, recipient)
  }

  def fromPacket[A: Unpickler: FastTypeTag](pkt: DatagramPacket): (Int, Short, A) = {
    val buffer = ByteBuffer.wrap(pkt.getData)
    val sequenceId = buffer.getInt()
    val senderId = buffer.getShort()
    val length = buffer.getInt()
    val bytes = Array.ofDim[Byte](length)
    buffer.get(bytes)
    val payload = BinaryPickle(bytes).unpickle[A]
    (sequenceId, senderId, payload)
  }

}
