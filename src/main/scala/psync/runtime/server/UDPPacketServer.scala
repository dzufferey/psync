package psync.runtime.server

import psync.runtime._
import psync.ProcessID
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
    _defaultHandler: Message => Unit,
    options: RuntimeOptions) extends PacketServer(executor, port, initGroup, _defaultHandler, options)
{

  protected var chan: Channel = null
  def channel: Channel = chan

  Logger.assert(options.protocol == NetworkProtocol.UDP, "UDPPacketServer", "transport layer: only UDP supported")

  def close {
    dispatcher.clear
    try {
      evtGroup.shutdownGracefully
    } finally {
      chan.close()
      chan = null
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

    b.handler(new UDPPacketServerHandler(defaultHandler, dispatcher))
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
    chan.writeAndFlush(pkt, chan.voidPromise())
  }

}

@Sharable
class UDPPacketServerHandler(
    defaultHandler: DatagramPacket => Unit,
    dispatcher: InstanceDispatcher
  ) extends SimpleChannelInboundHandler[DatagramPacket](false) {

  //in Netty version 5.0 will be called: channelRead0 will be messageReceived
  override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
    try {
      if (!dispatcher.dispatch(pkt))
        defaultHandler(pkt)
    } catch {
      case t: Throwable =>
        Logger("UDPPacketServerHandler", Warning, "got " + t)
    }
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
  }

}


