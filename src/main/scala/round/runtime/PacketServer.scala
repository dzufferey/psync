package round.runtime

import round._
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

class PacketServer(
    executor: java.util.concurrent.Executor,
    ports: Iterable[Int],
    initGroup: Group,
    _defaultHandler: Message => Unit, //defaultHandler is responsible for releasing the ByteBuf payload
    options: Map[String, String] = Map.empty)
{

  val directory = new Directory(initGroup)

  def defaultHandler(pkt: DatagramPacket) {
    val msg = new Message(pkt, directory.group)
    _defaultHandler(msg)
  }

  private val groupKind = options.getOrElse("group", "nio").toLowerCase
  private def epoll = groupKind == "epoll"
  private def nio = groupKind == "nio"
  private def oio = groupKind == "oio"
    
  if (options.getOrElse("transport layer", "udp").toLowerCase != "udp") {
    Logger("PacketServer", Warning, "transport layer: only UDP supported for the moment")
  }

  private val group: EventLoopGroup = {
    if (epoll) new EpollEventLoopGroup()
    else if (nio) new NioEventLoopGroup()
    else if (oio) new OioEventLoopGroup()
    else {
      Logger("PacketServer", Warning, "event group is unknown, using nio instead")
      new NioEventLoopGroup()
    }
  }

  private var chans: Array[Channel] = null
  def channels: Array[Channel] = chans

  val dispatcher = new InstanceDispatcher(options)

  def close {
    dispatcher.clear
    try {
      group.shutdownGracefully
    } finally {
      for ( i <- chans.indices) {
        if (chans(i) != null) {
          chans(i).close
          chans(i) = null
        }
      }
    }
  }

  def start {
    val b = new Bootstrap()
    b.group(group)
    if (epoll) b.channel(classOf[EpollDatagramChannel])
    else if (oio) b.channel(classOf[OioDatagramChannel])
    else b.channel(classOf[NioDatagramChannel])
    b.handler(new PackerServerHandler(defaultHandler, dispatcher))

    val ps = ports.toArray
    chans = ps.map( p => b.bind(p).sync().channel() )
  }

}

@Sharable
class PackerServerHandler(
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
        Logger("PacketServerHandler", Warning, "got " + t)
    }
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
  }

}


