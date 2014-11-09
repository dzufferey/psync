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
import io.netty.bootstrap.Bootstrap
import java.net.InetSocketAddress

class PacketServer(
    port: Int,
    initGroup: Group,
    defaultHandler: Message => Unit, //defaultHandler is responsible for releasing the ByteBuf payload
    options: Map[String, String] = Map.empty)
{

  val directory = new Directory(initGroup)

  val epoll = {
    val g = options.getOrElse("group", "nio").toLowerCase
    if (g == "epoll") {
      true
    } else if (g == "nio") {
      false
    } else {
      Logger("PacketServer", Warning, "event group is unknown, using nio instead")
      false
    }
  }
    
  if (options.getOrElse("transport layer", "udp").toLowerCase != "udp") {
     Logger("PacketServer", Warning, "transport layer: only UDP supported for the moment")
  }

  private val executor = java.util.concurrent.Executors.newCachedThreadPool()
  //private val executor = java.util.concurrent.Executors.newFixedThreadPool(8)

  private val group: EventLoopGroup =
    if (epoll) new EpollEventLoopGroup()
    else new NioEventLoopGroup()

  def submitTask[T](task: java.util.concurrent.Callable[T]) = {
    executor.submit(task)
  }

  private var chan: Channel = null
  def channel: Channel = chan

  val dispatcher = new InstanceDispatcher(executor, defaultHandler, directory)

  def close {
    dispatcher.clear
    try {
      group.shutdownGracefully
      executor.shutdownNow
    } finally {
      if (chan != null) {
        chan.close
        chan = null
      }
    }
  }

  def start {
    try {
      val b = new Bootstrap();
      b.group(group)
        .channel(if (epoll) classOf[EpollDatagramChannel]
                 else classOf[NioDatagramChannel])
        .handler(new PackerServerHandler(dispatcher))

      chan = b.bind(port).sync().channel()
      //chan.closeFuture().await() //closeFuture is a notification when the channel is closed
    } finally {
      //close
    }
  }

}

class PackerServerHandler(
    dispatcher: InstanceDispatcher
  ) extends SimpleChannelInboundHandler[DatagramPacket](false) {

  //in Netty version 5.0 will be called: channelRead0 will be messageReceived
  override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
    dispatcher.dispatch(pkt)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
  }

}


