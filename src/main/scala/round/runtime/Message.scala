package round.runtime

import round._

import io.netty.channel.socket.DatagramPacket
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import io.netty.buffer.ByteBuf

import scala.pickling._
import binary._

//TODO simplify to be just a ByteBufHolder ?

class Message(val packet: DatagramPacket, dir: Group){

  def payload: ByteBuf = packet.content
  def senderId: ProcessID = dir.self
  lazy val receiverId: ProcessID =  try { dir.inetToId(packet.sender) }
                                    catch { case _: Exception => new ProcessID(-1) }
  lazy val tag: Tag = new Tag(payload.getLong(0))

  def instance = tag.instanceNbr
  def round = tag.roundNbr
  
  def getContent[A: SPickler: Unpickler: FastTypeTag]: A = {
    val bytes = getPayLoad
    val converted = BinaryPickle(bytes).unpickle[A]
    converted
  }

  def getInt(idx: Int): Int = {
    payload.getInt(8+idx)
  }
  
  def getPayLoad: Array[Byte] = {
    val idx: Int = payload.readerIndex()
    payload.readLong() //skip the tag
    val length: Int = payload.readableBytes()
    val bytes = Array.ofDim[Byte](length)
    payload.readBytes(bytes)
    payload.readerIndex(idx)
    bytes
  }

  def release = payload.release

}


object Message {

  def getTag(buffer: ByteBuffer): Tag = {
    new Tag(buffer.getLong(0))
  }
  
  def getTag(buffer: ByteBuf): Tag = {
    new Tag(buffer.getLong(0))
  }


}
