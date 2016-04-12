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
    _defaultHandler: Message => Unit, //defaultHandler is responsible for releasing the ByteBuf payload
    options: RuntimeOptions) extends PacketServer(executor, port, initGroup, _defaultHandler, options)
{

  def defaultHandler(pkt: DatagramPacket) {
    val msg = new Message(pkt, directory.group)
    _defaultHandler(msg)
  }

  Logger.assert(options.protocol == NetworkProtocol.UDP, "UDPPacketServer", "transport layer: only UDP supported")

  private val group: EventLoopGroup = options.group match {
    case NetworkGroup.NIO => new NioEventLoopGroup()
    case NetworkGroup.OIO => new OioEventLoopGroup()
    case NetworkGroup.EPOLL => new EpollEventLoopGroup()
  }

  def close {
    dispatcher.clear
    try {
      group.shutdownGracefully
    } finally {
      chan.close()
      chan = null
    }
  }

  def start {
    val packetSize = options.packetSize
    val b = new Bootstrap()
    b.group(group)
    options.group match {
      case NetworkGroup.NIO =>   b.channel(classOf[NioDatagramChannel])
      case NetworkGroup.OIO =>   b.channel(classOf[OioDatagramChannel])
      case NetworkGroup.EPOLL => b.channel(classOf[EpollDatagramChannel])
    }

    if (packetSize >= 8) {//make sure we have at least space for the tag
      b.option[Integer](ChannelOption.SO_RCVBUF, packetSize)
      b.option(ChannelOption.RCVBUF_ALLOCATOR, new FixedRecvByteBufAllocator(packetSize))
    }

    b.handler(new UDPPacketServerHandler(defaultHandler, dispatcher))
    chan = b.bind(port).sync().channel()
  }

  def send(pkt: DatagramPacket) {
    chan.write(pkt, channel.voidPromise())
    chan.flush
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


