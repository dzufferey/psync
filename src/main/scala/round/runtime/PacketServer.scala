package round.runtime

import round._

import io.netty.buffer._
import io.netty.channel._
import io.netty.channel.socket._
import io.netty.channel.nio._
import io.netty.channel.socket.nio._
import io.netty.channel.epoll._
import io.netty.bootstrap.Bootstrap
import java.net.InetSocketAddress

import scala.pickling._

//TODO we don't want to do buzy waiting, need to do that asynchonously
//try using http://netty.io/
//for an UDP example:
//  https://github.com/netty/netty/tree/master/example/src/main/java/io/netty/example/qotm
//for a TCP example:
//  https://github.com/netty/netty/tree/master/example/src/main/java/io/netty/example/echo

class PacketServer(
    port: Int,
    initGroup: Group,
    defaultHandler: Message => Unit)
{

  val directory = new Directory(initGroup)

  val epoll = true //TODO read from config file

  private val group: EventLoopGroup =
    if (epoll) new EpollEventLoopGroup()
    else new NioEventLoopGroup()

  private var chan: Channel = null
  def channel: Channel = chan

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
      //chan.closeFuture().await() //closeFuture is a notification when the channel is closed
    } finally {
      //close
    }
  }

  def registerInstance(instanceHandler: PredicateLayer) {
    val p = channel.pipeline()
    p.addFirst(instanceHandler.instance.toString, instanceHandler)
  }
  
  def terminateInstance(inst: Short) {
    channel.pipeline().remove(inst.toString)
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
    defaultHandler(msg)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
  }

}


