package round.runtime

import round._

import io.netty.buffer._
import io.netty.channel._
import io.netty.channel.socket._
import io.netty.channel.nio._
import io.netty.channel.socket.nio._
import io.netty.channel.epoll._
import io.netty.bootstrap.Bootstrap

//how to combine this with the late/early replica
//the algorithm should start a server.
//upon reception of a message, the service must decide how to dispatch the message: round (which one), recovery, ??

//TODO we don't want to do buzy waiting, need to do that asynchonously
//try using http://netty.io/
//for an UDP server example:
// https://github.com/netty/netty/tree/master/example/src/main/java/io/netty/example/qotm


//send all in one go:
//http://normanmaurer.me/presentations/2014-facebook-eng-netty/slides.html#28.0
//to send use: Channel.write(msg, Channel.voidPromise())
//this does not create a Future, save some reseources

//for rounds we can add/remove handler ?

//TODO pack the message in ByteBufHolder ?

class PacketServer(port: Int) {

  val epoll = true //TODO read from config file

  /** this is blocking, finish when the channel is closed */
  def run {
    val group: EventLoopGroup =
      if (epoll) new EpollEventLoopGroup()
      else new NioEventLoopGroup()
    try {
        val b = new Bootstrap();
        b.group(group)
          .channel(if (epoll) classOf[EpollDatagramChannel]
                   else classOf[NioDatagramChannel])
          .handler(new PackerServerHandler())

        b.bind(port).sync().channel().closeFuture().await()
    } finally {
        group.shutdownGracefully()
    }
  }

}


class PackerServerHandler extends SimpleChannelInboundHandler[DatagramPacket] {

  //in Netty version 5.0 will be called: channelRead0 will be messageReceived
  override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
    println(pkt)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
  }

}


