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

//TODO we don't want to do buzy waiting, need to do that asynchonously
//try using http://netty.io/
//for an UDP example:
//  https://github.com/netty/netty/tree/master/example/src/main/java/io/netty/example/qotm
//for a TCP example:
//  https://github.com/netty/netty/tree/master/example/src/main/java/io/netty/example/echo

class PacketServer(
    port: Int,
    initGroup: Group,
    defaultHandler: Message => Unit,
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
      Logger("Predicate", Warning, "event group is unknown, using nio instead")
      false
    }
  }
    
  if (options.getOrElse("transport layer", "udp").toLowerCase != "udp") {
     Logger("Predicate", Warning, "transport layer: only UDP supported for the moment")
  }

  private val group: EventLoopGroup =
    if (epoll) new EpollEventLoopGroup()
    else new NioEventLoopGroup()

  private var chan: Channel = null
  def channel: Channel = chan

  val dispatcher = new InstanceDispatcher

  def close {
    try {
      if (chan != null) {
        chan.close
        chan = null
      }
    } finally {
      group.shutdownGracefully
    }
  }

  def start {
    try {
      val b = new Bootstrap();
      b.group(group)
        .channel(if (epoll) classOf[EpollDatagramChannel]
                 else classOf[NioDatagramChannel])
        .handler(new PackerServerHandler(directory, defaultHandler))

      chan = b.bind(port).sync().channel()
      //add the dispatcher
      chan.pipeline.addFirst("dispatcher", dispatcher)
      //chan.closeFuture().await() //closeFuture is a notification when the channel is closed
    } finally {
      //close
    }
  }

}

//defaultHanlder is responsible for releasing the ByteBuf payload
class PackerServerHandler(
    dir: Directory,
    defaultHandler: Message => Unit
  ) extends SimpleChannelInboundHandler[DatagramPacket](false) {

  //in Netty version 5.0 will be called: channelRead0 will be messageReceived
  override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
    val src = dir.inetToId(pkt.sender)
    val dst = dir.self //inetToId(pkt.recipient)
    val buf = pkt.content
    val msg = Message.wrapByteBuf(src, dst, buf)
    //is the default handler drop the message it cal lead to leak
    defaultHandler(msg)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
  }

}


