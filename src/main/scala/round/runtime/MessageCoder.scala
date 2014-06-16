package round.runtime

import io.netty.buffer._
import io.netty.channel._
import io.netty.channel.socket._
import java.net.InetSocketAddress

import scala.pickling._
import binary._

class MessageDecoder[A: SPickler: Unpickler: FastTypeTag](address: InetSocketAddress => Short)
    extends SimpleChannelInboundHandler[DatagramPacket] {

  //in Netty version 5.0 will be called: channelRead0 will be messageReceived
  override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
    val src = address(pkt.sender)
    val dst = address(pkt.recipient)
    val msg = Message.fromByteBuf(src, dst, pkt.content)
    pkt.release
    ctx.fireChannelRead(msg)
  }

}

class MessageEncoder[A: SPickler: FastTypeTag](address: Short => InetSocketAddress)
    extends ChannelOutboundHandlerAdapter {

  override def write( ctx: ChannelHandlerContext,
                      obj: Object,
                      promise: ChannelPromise) {
    val msg = obj.asInstanceOf[Message[A]]
    val dst = address(msg.receiverId)
    val src = address(msg.senderId)
    val pkt = new DatagramPacket(Unpooled.wrappedBuffer(msg.toByteBuffer()), dst, src)
    ctx.write(pkt, promise)
  }

}

