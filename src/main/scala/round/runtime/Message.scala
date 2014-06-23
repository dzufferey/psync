package round.runtime

import round.Algorithm._

import io.netty.channel.socket.DatagramPacket
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import io.netty.buffer.ByteBuf

import scala.pickling._
import binary._
// http://stackoverflow.com/questions/18725699/scala-pickling-and-type-parameters

//TODO simplify to be just a ByteBufHolder ?

class Message(
    val payload: ByteBuf,
    val senderId: ProcessID,
    val receiverId: ProcessID,
    val tag: Tag) {

  def instance = tag.instanceNbr
  def round = tag.roundNbr
  
  def repack(grp: Group, tag: Tag = new Tag(0)): DatagramPacket = {
    val src = grp.idToInet(senderId)
    val dst = grp.idToInet(receiverId)
    payload.setLong(0, tag.underlying)
    new DatagramPacket(payload, dst, src)
  }

  def getContent[A: SPickler: Unpickler: FastTypeTag]: A = {
    val idx: Int = payload.readerIndex()
    val length: Int = payload.readableBytes()
    val bytes = Array.ofDim[Byte](length)
    payload.readBytes(bytes)
    payload.readerIndex(idx)
    val converted = BinaryPickle(bytes).unpickle[A]
    converted
  }

}


object Message {

  def getTag(buffer: ByteBuffer): Tag = {
    new Tag(buffer.getLong(0))
  }
  
  def getTag(buffer: ByteBuf): Tag = {
    new Tag(buffer.getLong(0))
  }

  def wrapByteBuf(
        sender: ProcessID,
        receiver: ProcessID,
        buffer: ByteBuf
      ): Message = {
    val tag = new Tag(buffer.readLong())
    val senderId = buffer.readShort()
    val length = buffer.readInt()
    val payload = buffer.slice(buffer.readerIndex, length)
    new Message(payload, sender, receiver, tag)
  }


}
