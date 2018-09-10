package psync.runtime

import psync._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import io.netty.buffer._
import io.netty.channel._
import io.netty.channel.socket._
import io.netty.channel.nio._
import io.netty.channel.socket.nio._
import io.netty.channel.epoll._
import io.netty.channel.oio._
import io.netty.channel.socket.oio._
import io.netty.channel.ChannelHandler.Sharable
import io.netty.bootstrap.Bootstrap
import java.net.InetSocketAddress

class UDPPacketServer(
    executor: java.util.concurrent.Executor,
    port: Int,
    initGroup: Group,
    defaultHandler: Message => Unit,
    options: RuntimeOptions) extends PacketServer(executor, port, initGroup, defaultHandler, options)
{

  protected var chan: Channel = null
  def channel: Channel = chan

  Logger.assert(options.protocol == NetworkProtocol.UDP, "UDPPacketServer", "transport layer: only UDP supported")

  def close {
    dispatcher.clear
    try {
      evtGroup.shutdownGracefully
    } finally {
      val c = chan
      chan = null
      c.close()
    }
  }

  def start {
    val packetSize = options.packetSize
    val b = new Bootstrap()
    b.group(evtGroup)
    options.group match {
      case NetworkGroup.NIO =>   b.channel(classOf[NioDatagramChannel])
      case NetworkGroup.OIO =>   b.channel(classOf[OioDatagramChannel])
      case NetworkGroup.EPOLL => b.channel(classOf[EpollDatagramChannel])
    }

    b.option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT); //make sure we use the default pooled allocator

    if (packetSize >= 8) {//make sure we have at least space for the tag
      b.option[Integer](ChannelOption.SO_RCVBUF, packetSize)
      b.option(ChannelOption.RCVBUF_ALLOCATOR, new FixedRecvByteBufAllocator(packetSize))
    }

    b.handler(new UDPPacketServerHandler(handlePacket))
    chan = b.bind(port).sync().channel()
  }

  def send(pid: ProcessID, buf: ByteBuf) {
    val grp = group
    val dst = grp.idToInet(pid)
    val pkt =
      if (grp.contains(grp.self)) {
        val src = grp.idToInet(grp.self)
        new DatagramPacket(buf, dst, src)
      } else {
        new DatagramPacket(buf, dst)
      }
    if (chan != null) {
      chan.writeAndFlush(pkt, chan.voidPromise())
    }
  }

  def handlePacket(dp: DatagramPacket) {
    val msg = new Message(dp, group)
    dispatch(msg)
  }

}

@Sharable
class UDPPacketServerHandler(handlePacket: DatagramPacket => Unit) extends SimpleChannelInboundHandler[DatagramPacket](false) {

  //in Netty version 5.0 will be called: channelRead0 will be messageReceived
  override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
    try {
      handlePacket(pkt)
    } catch {
      case t: Throwable =>
        Logger("UDPPacketServerHandler", Error, "got " + t + "\n  " + t.getStackTrace.mkString("\n  "))
    }
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
  }

}


