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

class Message(val receiver: ProcessID, val sender: ProcessID, val payload: ByteBuf) {

  def this(pkt: DatagramPacket, dir: Group) = {
    this(dir.self, dir.inetToIdOrDefault(pkt.sender), pkt.content)
    payload.retain
    pkt.release
  }

  assert(payload.readerIndex == 0)

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
    payload.readerIndex(tag.size)
    val kryoIn = new KryoByteBufInput(payload)
    val result = kryo.readObject(kryoIn, implicitly[ClassTag[A]].runtimeClass).asInstanceOf[A]
    payload.readerIndex(0)
    result
  }

  def getInt(idx: Int): Int = {
    payload.getInt(tag.size + idx)
  }
  
  def getPayLoad: Array[Byte] = {
    payload.readerIndex(tag.size)
    val length: Int = payload.readableBytes()
    val bytes = Array.ofDim[Byte](length)
    payload.readBytes(bytes)
    payload.readerIndex(0)
    bytes
  }

  def bufferAfterTag: ByteBuf = {
    payload.readerIndex(tag.size)
    payload
  }

  def release = {
    payload.release
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
    buffer.readerIndex(tag.size)
    buffer
  }

  // The tag is only needed to the size. It is not written.
  def setContent[A: KryoRegistration](tag:Tag, buffer: ByteBuf, value: A) {
    val kryo = KryoSerializer.serializer
    implicitly[KryoRegistration[A]].register(kryo)
    setContent(kryo, tag, buffer, value)
  }

  // The tag is only needed to the size. It is not written.
  def setContent[A](kryo: Kryo, tag:Tag, buffer: ByteBuf, value: A) {
    buffer.writerIndex(tag.size)
    val kryoOut = new KryoByteBufOutput(buffer)
    kryo.writeObject(kryoOut, value)
  }

  implicit object MessageOrdering extends Ordering[Message] {
    def compare(a: Message, b: Message): Int = a.round - b.round
  }


}
