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

class UdpRuntime(o: RuntimeOptions, dh: Message => Unit) extends Runtime(o, dh) {

  protected var chan: Channel = null
  def channel: Channel = chan

  Logger.assert(options.protocol == NetworkProtocol.UDP, "UdpRuntime", "transport layer: only UDP supported")

  def closeServer: Unit = {
    dispatcher.clear
    try {
      evtGroup.shutdownGracefully
    } finally {
      val c = chan
      chan = null
      c.close()
    }
  }

  def startServer: Unit = {
    val packetSize = options.packetSize
    val b = new Bootstrap()
    b.group(evtGroup)
    options.group match {
      case NetworkGroup.NIO =>   b.channel(classOf[NioDatagramChannel])
      case NetworkGroup.EPOLL => b.channel(classOf[EpollDatagramChannel])
    }

    b.option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT); //make sure we use the default pooled allocator

    if (packetSize >= 8) {//make sure we have at least space for the tag
      b.option[Integer](ChannelOption.SO_RCVBUF, packetSize)
      b.option(ChannelOption.RCVBUF_ALLOCATOR, new FixedRecvByteBufAllocator(packetSize))
    }

    b.handler(new UDPPacketServerHandler(this))
    chan = b.bind(port).sync().channel()
  }

  protected[psync] def send(pid: ProcessID, buf: ByteBuf): Unit = {
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

  def handlePacket(dp: DatagramPacket): Unit = {
    val msg = new Message(dp, group)
    dispatch(msg)
  }

}

@Sharable
class UDPPacketServerHandler(rt: UdpRuntime) extends SimpleChannelInboundHandler[DatagramPacket](false) {

  //in Netty version 5.0 will be called: channelRead0 will be messageReceived
  override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket): Unit = {
    try {
      rt.handlePacket(pkt)
    } catch {
      case t: Throwable =>
        Logger("UDPPacketServerHandler", Error, "got " + t + "\n  " + t.getStackTrace.mkString("\n  "))
    }
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    cause.printStackTrace()
  }

}


