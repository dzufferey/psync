package psync.runtime

import psync._

import io.netty.channel.socket.DatagramPacket
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import io.netty.buffer.ByteBuf

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import scala.pickling._
import scala.pickling.Defaults._
import binary._

class Message(val packet: DatagramPacket, dir: Group){

  def payload: ByteBuf = packet.content
  def receiverId: ProcessID = dir.self
  lazy val senderId: ProcessID =  try { dir.inetToId(packet.sender) }
                                  catch { case _: Exception => new ProcessID(-1) }
  val tag: Tag = new Tag(payload.getLong(0))

  private var collected = false

  def flag = tag.flag
  def instance = tag.instanceNbr
  def round = tag.roundNbr
  
  def getContent[A: Pickler: Unpickler: FastTypeTag]: A = {
    val idx: Int = payload.readerIndex()
    payload.readerIndex(idx + tag.size)
    val pickle = BinaryPickle(new psync.macros.ByteBufInput(payload))
    val result = pickle.unpickle[A]
    payload.readerIndex(idx)
    result
  }

  def getInt(idx: Int): Int = {
    payload.getInt(tag.size + idx)
  }
  
  def getPayLoad: Array[Byte] = {
    val idx: Int = payload.readerIndex()
    payload.readerIndex(idx + tag.size)
    val length: Int = payload.readableBytes()
    val bytes = Array.ofDim[Byte](length)
    payload.readBytes(bytes)
    payload.readerIndex(idx)
    bytes
  }

  def bufferAfterTag: ByteBuf = {
    val idx: Int = payload.readerIndex()
    payload.readerIndex(idx + tag.size)
    payload
  }

  def release = {
    packet.release
    collected = true
  }

  override def finalize() {
    if (!collected) {
      Logger("Message", Warning, "message not collected")
    }
  }

}


object Message {

  def getInstance(buffer: ByteBuffer) = getTag(buffer).instanceNbr

  def getTag(buffer: ByteBuffer): Tag = {
    new Tag(buffer.getLong(0))
  }
  
  def getTag(buffer: ByteBuf): Tag = {
    new Tag(buffer.getLong(0))
  }

  def moveAfterTag(buffer: ByteBuf): ByteBuf = {
    val tag = getTag(buffer)
    val idx: Int = buffer.readerIndex()
    buffer.readerIndex(idx + tag.size)
    buffer
  }

}
