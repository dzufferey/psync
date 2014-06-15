package round.runtime

import io.netty.buffer._
import io.netty.channel._
import io.netty.channel.socket._
import java.net.InetSocketAddress

import scala.pickling._
import binary._

class MessageDecoder[A: SPickler: Unpickler: FastTypeTag](self: Short)
    extends SimpleChannelInboundHandler[DatagramPacket] {

  //in Netty version 5.0 will be called: channelRead0 will be messageReceived
  override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
    val msg = Message.fromByteBuf(self, pkt.content)
    pkt.release
    ctx.fireChannelRead(msg)
  }

}

class MessageEncoder[A: SPickler: FastTypeTag](address: Map[Short, InetSocketAddress]) extends ChannelOutboundHandlerAdapter {

  override def write( ctx: ChannelHandlerContext,
                      obj: Object,
                      promise: ChannelPromise) {
    val msg = obj.asInstanceOf[Message[A]]
    val dst = address(msg.receiverId)
    val pkt = new DatagramPacket(Unpooled.wrappedBuffer(msg.toByteBuffer()), dst)
    ctx.write(pkt, promise)
  }

}

