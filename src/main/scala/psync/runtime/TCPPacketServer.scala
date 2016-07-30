package psync.runtime

import psync._
import psync.utils.Timer
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import io.netty.bootstrap._
import io.netty.buffer._
import io.netty.channel._
import io.netty.channel.socket._
import io.netty.channel.nio._
import io.netty.channel.socket.nio._
import io.netty.channel.epoll._
import io.netty.channel.oio._
import io.netty.channel.socket.oio._
import io.netty.channel.ChannelHandler.Sharable
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder,LengthFieldPrepender}
import io.netty.handler.ssl._
import io.netty.handler.ssl.util._
import java.net.{InetSocketAddress,InetAddress}
import java.util.concurrent.{Executors, TimeUnit}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{Map, SynchronizedMap, HashMap}
import java.nio.charset.StandardCharsets.UTF_8

class TCPPacketServer(
    executor: java.util.concurrent.Executor,
    port: Int,
    initGroup: Group,
    _defaultHandler: Message => Unit,
    options: RuntimeOptions) extends PacketServer(executor, port, initGroup, _defaultHandler, options)
{

  val recipientMap: Map[ProcessID, Channel] = new TrieMap
  val unknownSender = new java.util.concurrent.ConcurrentLinkedQueue[(InetSocketAddress,Channel)]
  protected[runtime] def addChannel(addr: InetSocketAddress, chan: Channel) {
    group.getSafe(addr) match {
      case Some(r) =>
        recipientMap.update(r.id, chan)
      case None =>
        unknownSender.add(addr -> chan)
    }
  }
  protected[runtime] def removeChannel(addr: InetSocketAddress) {
    group.getSafe(addr) match {
      case Some(r) =>
        recipientMap.remove(r.id)
      case None =>
        Logger("TCPPacketServer", Debug, "No channel to remove for " + addr)
    }
  }

  Logger.assert(options.protocol == NetworkProtocol.TCP || options.protocol == NetworkProtocol.TCP_SSL,
                "TCPPacketServer", "transport layer: only TCP supported")

  private val isSSLEnabled = (options.protocol == NetworkProtocol.TCP_SSL)

  private val sslServerCtx = if (isSSLEnabled) {
    val ssc = new SelfSignedCertificate()
    SslContextBuilder.forServer(ssc.certificate(), ssc.privateKey()).build();
  } else null

  private val sslClientCtx = if (isSSLEnabled) {
    SslContextBuilder.forClient().trustManager(InsecureTrustManagerFactory.INSTANCE).build();
  } else null

  override def group_=(grp: Group) {
    val oldGroup = grp
    super.group_=(grp)
    //TODO concurrent
    val snapShot = recipientMap.toSeq
    recipientMap.clear
    // the old channed reuse or close
    snapShot.foreach{ case (id, chan) =>
      val addr = oldGroup.get(id).netAddress
      grp.getSafe(addr) match {
        case Some(id2) =>
          recipientMap.update(id2.id, chan)
        case None =>
          //not needed anymore
          chan.close
      }
    }
    // check if pending
    var ac = unknownSender.poll
    while (ac != null) {
      val (addr, chan) = ac
      grp.getSafe(addr) match {
        case Some(id2) =>
          recipientMap.update(id2.id, chan)
        case None =>
          //not needed
          chan.close
      }
      ac = unknownSender.poll
    }
    // start connecting the new guys
    grp.others.foreach( replica => {
      if (!recipientMap.contains(replica.id)) {
        startConnection(replica)
      }
    })
  }

  protected def getAddress: InetSocketAddress = {
    if (group contains group.self) {
      val addr = group.idToInet(group.self)
      assert(addr.getPort == port)
      addr
    } else {
      new InetSocketAddress(InetAddress.getLocalHost(), port)
    }
  }

  def close {
    dispatcher.clear
    try {
      evtGroup.shutdownGracefully
    } finally {
      recipientMap.foreach {
        case (_, channel) =>
          channel.close()
      }
    }
  }

  def startListener() {
    val bootstrap = new ServerBootstrap()
    bootstrap.group(evtGroup)
    options.group match {
      case NetworkGroup.NIO =>   bootstrap.channel(classOf[NioServerSocketChannel])
      case NetworkGroup.OIO =>   bootstrap.channel(classOf[OioServerSocketChannel])
      case NetworkGroup.EPOLL => bootstrap.channel(classOf[EpollServerSocketChannel])
    }
    bootstrap.option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT); //make sure we use the default pooled allocator
    bootstrap.childHandler(new ChannelInitializer[SocketChannel] {
      override def initChannel(ch: SocketChannel) {
        val pipeline = ch.pipeline()
        if (isSSLEnabled) {
          pipeline.addLast(sslServerCtx.newHandler(ch.alloc()))
        } else {
          pipeline.addLast(
            new LengthFieldBasedFrameDecoder(Short.MaxValue, 0, 2, 0, 2),
            new LengthFieldPrepender(2)
          )
        }
        val srv = TCPPacketServer.this
        val unk = options.acceptUnknownConnection
        pipeline.addLast(new TCPPacketServerHandler(srv, getAddress, unk))
      }
    })
    bootstrap.bind(port).sync()
  }

  def delayedStartConnection(replica: Replica) {
    if (!evtGroup.isShuttingDown()) {
      val loop = evtGroup.next()
      loop.schedule(new Runnable() {
        override def run() {
          startConnection(replica)
        }
      }, options.connectionRestartPeriod, TimeUnit.MILLISECONDS)
    }
  }

  def startConnection(replica: Replica) {
    // check if replica still in the group
    if (group.getSafe(replica.netAddress).isEmpty) {
      return // the group has changed
    }
    //
    val bootstrap = new Bootstrap()
    val inet = replica.netAddress
    bootstrap.group(evtGroup)
    options.group match {
      case NetworkGroup.NIO =>   bootstrap.channel(classOf[NioSocketChannel])
      case NetworkGroup.OIO =>   bootstrap.channel(classOf[OioSocketChannel])
      case NetworkGroup.EPOLL => bootstrap.channel(classOf[EpollSocketChannel])
    }
    bootstrap.option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT); //make sure we use the default pooled allocator
    bootstrap.handler(new ChannelInitializer[SocketChannel] {
      override def initChannel(ch: SocketChannel) {
        val pipeline = ch.pipeline()
        if (isSSLEnabled) {
          pipeline.addLast(sslClientCtx.newHandler(ch.alloc(), inet.getHostName(), inet.getPort()))
        } else {
          pipeline.addLast(
            new LengthFieldBasedFrameDecoder(Short.MaxValue, 0, 2, 0, 2),
            new LengthFieldPrepender(2)
          )
        }
        pipeline.addLast(new TCPPacketClientHandler(TCPPacketServer.this, getAddress, replica))
      }
    })
    bootstrap.connect(inet).addListener(new ChannelFutureListener() {
      override def operationComplete(future: ChannelFuture) {
        if (future.cause() != null) {
          Logger("TCPPacketServer", Warning, "Couldn't connect, trying again...")
          delayedStartConnection(replica)
        }
      }
    })
  }

  def start {
    startListener()
    group.others.foreach { recipient =>
      if (directory.self.id < recipient.id.id)
        startConnection(recipient)
    }
    Thread.sleep(1000)
  }

  def send(to: ProcessID, buf: ByteBuf) {
    val chanOption = recipientMap.get(to)
    chanOption match {
      case Some(chan) =>
        chan.writeAndFlush(buf, chan.voidPromise())
      case None =>
        Logger("TCPPacketServer", Info, "Tried to send packet, but no channel was available.")
    }
  }

}

class TCPPacketServerHandler(
    packetServer: TCPPacketServer,
    localAddress: InetSocketAddress,
    acceptUnknownConnection: Boolean
  ) extends SimpleChannelInboundHandler[ByteBuf](false) {

  var remoteAddress: InetSocketAddress = null

  override def channelActive(ctx: ChannelHandlerContext) {
    Logger("TCPPacketServerHandler", Debug, "Someone connected to " + localAddress)
  }

  override def channelInactive(ctx: ChannelHandlerContext) {
    Logger("TCPPacketServerHandler", Debug, "Someone disconneted")
    if (remoteAddress != null)
      packetServer.removeChannel(remoteAddress)
  }

  protected def readAddress(buf: ByteBuf): Option[InetSocketAddress] = {
    val size = buf.readInt
    val string = buf.readCharSequence(size, UTF_8).toString
    buf.release
    val split = string.lastIndexOf(':')
    if (split < 0) {
      None
    } else {
      try {
        val host = string.substring(0, split)
        val port = string.substring(split + 1).toInt
        val address = new InetSocketAddress(host, port)
        Some(address)
      } catch {
        case t: Throwable =>
          None
      }
    }
  }

  override def channelRead0(ctx: ChannelHandlerContext, buf: ByteBuf) {
    if (remoteAddress == null) {
      // First message is an InetSocketAddress
      readAddress(buf) match {
        case Some(addr) =>
          Logger("TCPPacketServerHandler", Info, {
            val selfId = packetServer.group.self.id
            addr + " is connecting to " + selfId + "(" + localAddress + ")" })
          remoteAddress = addr
          packetServer.group.getSafe(remoteAddress) match {
            case Some(replica) =>
              packetServer.addChannel(remoteAddress, ctx.channel())
              Logger("TCPPacketServerHandler", Info, {
                val selfId = packetServer.group.self.id
                "<- Client " + replica.id.id + " connected to Server " + selfId })
            case None =>
              if (acceptUnknownConnection) {
                packetServer.addChannel(remoteAddress, ctx.channel())
                Logger("TCPPacketServerHandler", Info, {
                  val selfId = packetServer.group.self.id
                  "Client " + remoteAddress + " connected to Server " + selfId })
              } else {
                ctx.close()
                Logger("TCPPacketServerHandler", Info, "unknown connection " + remoteAddress + " closing.")
              }
          }
        case None =>
          Logger("TCPPacketServerHandler", Info, "failed to parse/resolve connecting address, closing.")
          ctx.close()
      }
    } else {
      //normal protocol messages
      val pkt = new DatagramPacket(buf, localAddress, remoteAddress)
      try {
        if (!packetServer.dispatcher.dispatch(pkt))
          packetServer.defaultHandler(pkt)
      } catch {
        case t: Throwable =>
          Logger("TCPPacketServerHandler", Warning, "got " + t + "\n  " + t.getStackTrace.mkString("\n  "))
      }
    }
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
  }
}

class TCPPacketClientHandler(
    packetServer: TCPPacketServer,
    localAddress: InetSocketAddress,
    remote: Replica
  ) extends SimpleChannelInboundHandler[ByteBuf](false) {

  override def channelRead0(ctx: ChannelHandlerContext, buf: ByteBuf) {
    val pkt = new DatagramPacket(buf, localAddress, remote.netAddress)
    try {
      if (!packetServer.dispatcher.dispatch(pkt))
        packetServer.defaultHandler(pkt)
    } catch {
      case t: Throwable =>
        Logger("TCPPacketClientHandler", Warning, "got " + t + "\n  " + t.getStackTrace.mkString("\n  "))
    }
  }

  override def channelActive(ctx: ChannelHandlerContext) {
    Logger("TCPPacketClientHandler", Debug, "connecting from " + localAddress + " to " + remote)
    val payload = ctx.alloc().buffer()
    payload.writeInt(-1)
    val length = payload.writeCharSequence(localAddress.toString, UTF_8)
    payload.setInt(0, length)
    val chan = ctx.channel()
    chan.writeAndFlush(payload, chan.voidPromise())
    packetServer.addChannel(remote.netAddress, chan)
    Logger("TCPPacketClientHandler", Info, "-> Client " + packetServer.group.self.id + " connected to " + remote.id.id)
  }

  override def channelInactive(ctx: ChannelHandlerContext) {
    Logger("TCPPacketClientHandler", Debug, "Someone disconneted.")
    packetServer.removeChannel(remote.netAddress)
    packetServer.delayedStartConnection(remote)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
  }
}

