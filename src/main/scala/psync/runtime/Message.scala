package psync.runtime

import psync._

import io.netty.channel.socket.DatagramPacket
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import io.netty.buffer.ByteBuf
import psync.utils.serialization.{KryoRegistration, KryoSerializer, KryoByteBufInput, KryoByteBufOutput}
import com.esotericsoftware.kryo.Kryo
import scala.reflect.ClassTag

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

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
  
  def getContent[A: ClassTag: KryoRegistration]: A = {
    val kryo = KryoSerializer.serializer
    implicitly[KryoRegistration[A]].register(kryo)
    getContent[A](kryo)
  }

  def getContent[A: ClassTag](kryo: Kryo): A = {
    val idx: Int = payload.readerIndex()
    payload.readerIndex(idx + tag.size)
    val kryoIn = new KryoByteBufInput(payload)
    val result = kryo.readObject(kryoIn, implicitly[ClassTag[A]].runtimeClass).asInstanceOf[A]
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

  def setContent[A: KryoRegistration](tag:Tag, buffer: ByteBuf, value: A) {
    val kryo = KryoSerializer.serializer
    implicitly[KryoRegistration[A]].register(kryo)
    setContent(kryo, tag, buffer, value)
  }

  def setContent[A](kryo: Kryo, tag:Tag, buffer: ByteBuf, value: A) {
    val idx: Int = buffer.writerIndex()
    buffer.writerIndex(idx + tag.size)
    val kryoOut = new KryoByteBufOutput(buffer)
    kryo.writeObject(kryoOut, value)
    buffer.writerIndex(idx)
  }

}
