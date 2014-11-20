package tests

import io.netty.buffer._
import io.netty.channel._
import io.netty.channel.socket._
import io.netty.channel.nio._
import io.netty.channel.socket.nio._
import io.netty.channel.epoll._
import io.netty.bootstrap.Bootstrap
import io.netty.buffer.PooledByteBufAllocator
import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicLong

class NettyServer(port: Int, options: Map[String, String] = Map.empty)
{

  val nbr = new AtomicLong(0l)

  val epoll = {
    val g = options.getOrElse("group", "nio").toLowerCase
    if (g == "epoll") {
      true
    } else {
      false
    }
  }
    
  private val group: EventLoopGroup =
    if (epoll) new EpollEventLoopGroup()
    else new NioEventLoopGroup()

  private var chan: Channel = null
  def channel: Channel = chan

  def close {
    try {
      group.shutdownGracefully
    } finally {
      if (chan != null) {
        chan.close
        chan = null
      }
    }
  }

  def start {
    val b = new Bootstrap();
    b.group(group)
      .channel(if (epoll) classOf[EpollDatagramChannel]
               else classOf[NioDatagramChannel])
      .handler(new PackerServerHandler())

    chan = b.bind(port).sync().channel()
  }

  def shutdown: Long = {
    close
    nbr.get
  }

  class PackerServerHandler() extends SimpleChannelInboundHandler[DatagramPacket](false) {

    override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
      nbr.incrementAndGet
      pkt.release
    }

    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
      cause.printStackTrace()
    }

  }

}

class NettyClient(clientPort: Int, serverPort: Int, options: Map[String, String] = Map.empty)
{

  val epoll = {
    val g = options.getOrElse("group", "nio").toLowerCase
    if (g == "epoll") {
      true
    } else {
      false
    }
  }
    
  private val group: EventLoopGroup =
    if (epoll) new EpollEventLoopGroup()
    else new NioEventLoopGroup()

  private var chan: Channel = null
  def channel: Channel = chan

  def close {
    try {
      group.shutdownGracefully
    } finally {
      if (chan != null) {
        chan.close
        chan = null
      }
    }
  }
  
  def start {
    val b = new Bootstrap();
    b.group(group)
      .channel(if (epoll) classOf[EpollDatagramChannel]
               else classOf[NioDatagramChannel])
      .handler(new throwAway())

    chan = b.bind(clientPort).sync().channel()
  }

  class throwAway() extends SimpleChannelInboundHandler[DatagramPacket](false) {

    override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
      pkt.release
    }

    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
      cause.printStackTrace()
    }

  }

  def loop {
    val addr = new InetSocketAddress("127.0.0.1", serverPort)
    while(true) {
      val payload = PooledByteBufAllocator.DEFAULT.buffer()
      payload.writeLong(0)
      payload.writeLong(0)
      payload.writeLong(0)
      val pkt = new DatagramPacket(payload, addr)
      channel.writeAndFlush(pkt).sync()
    }
  }

}

object Netty {

  var srv = true
  var s: NettyServer = null
  var c: NettyClient = null
  var begin = 0l
  
  def main(args: Array[java.lang.String]) {
    if (args.length == 1)
      srv = true
    else if (args.length == 2)
      srv = false
    else
      sys.error("wrong # of args")
    
    if (srv) {
      s = new NettyServer(args(0).toInt)
      s.start
      begin = java.lang.System.currentTimeMillis()
    } else {
      c = new NettyClient(args(0).toInt, args(1).toInt)
      c.start
      c.loop
    }

  }

  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        if (srv) {
          val versionNbr = s.shutdown
          val end = java.lang.System.currentTimeMillis()
          val duration = (end - begin) / 1000
          println("#pkt = " + versionNbr + ", Δt = " + duration + ", throughput = " + (versionNbr/duration))
        } else {
          c.close
        }
      }
    }
  )

}
