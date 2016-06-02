package psync.runtime

import psync._

import io.netty.channel.socket.DatagramPacket
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import io.netty.buffer.ByteBuf

import scala.pickling._
import scala.pickling.Defaults._
import binary._

class Message(val packet: DatagramPacket, dir: Group){

  def payload: ByteBuf = packet.content
  def receiverId: ProcessID = dir.self
  lazy val senderId: ProcessID =  try { dir.inetToId(packet.sender) }
                                    catch { case _: Exception => new ProcessID(-1) }
  lazy val tag: Tag = new Tag(payload.getLong(0))

  def flag = tag.flag
  def instance = tag.instanceNbr
  def round = tag.roundNbr
  
  def getContent[A: Pickler: Unpickler: FastTypeTag]: A = {
    val idx: Int = payload.readerIndex()
    payload.readerIndex(idx + Tag.size)
    val pickle = BinaryPickle(new psync.macros.ByteBufInput(payload))
    val result = pickle.unpickle[A]
    payload.readerIndex(idx)
    result
  }

  def getInt(idx: Int): Int = {
    payload.getInt(8+idx)
  }
  
  def getPayLoad: Array[Byte] = {
    val idx: Int = payload.readerIndex()
    payload.readerIndex(idx + Tag.size)
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
