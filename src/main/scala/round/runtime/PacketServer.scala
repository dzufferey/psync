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
    ports: Iterable[Int],
    initGroup: Group,
    defaultHandler: Message => Unit, //defaultHandler is responsible for releasing the ByteBuf payload
    options: Map[String, String] = Map.empty)
{

  val directory = new Directory(initGroup)

  private val groupKind = options.getOrElse("group", "nio").toLowerCase
  private def epoll = groupKind == "epoll"
  private def nio = groupKind == "nio"
  private def oio = groupKind == "oio"
    
  if (options.getOrElse("transport layer", "udp").toLowerCase != "udp") {
     Logger("PacketServer", Warning, "transport layer: only UDP supported for the moment")
  }

  private val executor = {
    options.get("workers") match {
      case Some(n) =>
        val cores = java.lang.Runtime.getRuntime().availableProcessors()
        val w = try {
          if (n endsWith "x") {
            val coeff = n.substring(0, n.length -1).toInt
            coeff * cores
          } else {
            n.toInt
          }
        } catch {
          case e: Exception =>
            Logger("PacketServer", Warning, "size of pool of workers has wrong format, using " + cores)
            cores
        }
        Logger("PacketServer", Debug, "using fixed thread pool of size " + w)
        java.util.concurrent.Executors.newFixedThreadPool(w)
      case None =>
        Logger("PacketServer", Debug, "using cached thread pool")
        java.util.concurrent.Executors.newCachedThreadPool()
    }
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

  def submitTask[T](task: java.util.concurrent.Callable[T]) = {
    executor.submit(task)
  }

  private var chans: Array[Channel] = null
  def channels: Array[Channel] = chans

  val dispatcher = new InstanceDispatcher(executor, defaultHandler, directory)

  def close {
    dispatcher.clear
    try {
      group.shutdownGracefully
      executor.shutdownNow
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
    b.handler(new PackerServerHandler(dispatcher))

    val ps = ports.toArray
    chans = ps.map( p => b.bind(p).sync().channel() )
  }

}

@Sharable
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


