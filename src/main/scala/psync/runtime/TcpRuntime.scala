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
import scala.collection.mutable.Map
import java.nio.charset.StandardCharsets.UTF_8

class TcpRuntime(o: RuntimeOptions, dh: Message => Unit) extends Runtime(o, dh) {

  val recipientMap: Map[ProcessID, Channel] = new TrieMap
  val unknownSender: Map[InetSocketAddress, Channel] = new TrieMap
  val handlers: Map[TCPPacketHandler,Unit] = new TrieMap
  protected[runtime] def addChannel(id: ProcessID, chan: Channel): Unit = {
    recipientMap.update(id, chan)
  }
  protected[runtime] def removeChannel(id: ProcessID): Unit = {
    recipientMap.remove(id)
  }
  protected[runtime] def addUnknownChannel(addr: InetSocketAddress, chan: Channel): Unit = {
    unknownSender.update(addr, chan)
  }
  protected[runtime] def removeUnknownChannel(addr: InetSocketAddress): Unit = {
    unknownSender.remove(addr)
  }
  protected[runtime] def addHandler(hdl: TCPPacketHandler): Unit = {
    handlers.update(hdl,())
  }
  protected[runtime] def removeHandler(hdl: TCPPacketHandler): Unit = {
    handlers.remove(hdl)
  }

  Logger.assert(options.protocol == NetworkProtocol.TCP || options.protocol == NetworkProtocol.TCP_SSL,
                "TcpRuntime", "transport layer: only TCP supported")

  private def isSSLEnabled = (options.protocol == NetworkProtocol.TCP_SSL)

  private def sslServerCtx = {
    assert(isSSLEnabled)
    if (o.sslContextForServer != null) {
      o.sslContextForServer
    } else {
      val ssc = new SelfSignedCertificate()
      SslContextBuilder.forServer(ssc.certificate(), ssc.privateKey()).build()
    }
  }

  private def sslClientCtx = {
    assert(isSSLEnabled)
    if (o.sslContextForClient != null) {
      o.sslContextForClient
    } else {
      SslContextBuilder.forClient().trustManager(InsecureTrustManagerFactory.INSTANCE).build()
    }
  }

  override def group_=(grp: Group): Unit = {
    val oldGroup = grp
    super.group_=(grp)
    //TODO concurrent
    handlers.foreach{ case (h,_) => h.rename(grp) }
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
    unknownSender.foreach{ case (addr, chan) =>
      grp.getSafe(addr) match {
        case Some(id2) =>
          recipientMap.update(id2.id, chan)
        case None =>
          //not needed
          chan.close
      }
    }
    unknownSender.clear
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

  def closeServer: Unit = {
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

  def startListener(): Unit = {
    val bootstrap = new ServerBootstrap()
    bootstrap.group(evtGroup)
    options.group match {
      case NetworkGroup.NIO =>   bootstrap.channel(classOf[NioServerSocketChannel])
      case NetworkGroup.EPOLL => bootstrap.channel(classOf[EpollServerSocketChannel])
    }
    bootstrap.option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT) //make sure we use the default pooled allocator
    bootstrap.childOption(ChannelOption.TCP_NODELAY, java.lang.Boolean.TRUE)
    bootstrap.childHandler(new ChannelInitializer[SocketChannel] {
      override def initChannel(ch: SocketChannel): Unit = {
        val pipeline = ch.pipeline()
        if (isSSLEnabled) {
          pipeline.addLast(sslServerCtx.newHandler(ch.alloc()))
        } else {
          pipeline.addLast(
            new LengthFieldBasedFrameDecoder(Short.MaxValue, 0, 2, 0, 2),
            new LengthFieldPrepender(2)
          )
        }
        val srv = TcpRuntime.this
        val unk = options.acceptUnknownConnection
        pipeline.addLast(new TCPPacketServerHandler(srv, group.self, unk))
      }
    })
    bootstrap.bind(port).sync()
  }

  def delayedStartConnection(replica: Replica): Unit = {
    if (!evtGroup.isShuttingDown()) {
      val loop = evtGroup.next()
      loop.schedule(new Runnable() {
        override def run(): Unit = {
          startConnection(replica)
        }
      }, options.connectionRestartPeriod, TimeUnit.MILLISECONDS)
    }
  }

  def startConnection(replica: Replica): Unit = {
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
      case NetworkGroup.EPOLL => bootstrap.channel(classOf[EpollSocketChannel])
    }
    bootstrap.option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT); //make sure we use the default pooled allocator
    bootstrap.option(ChannelOption.TCP_NODELAY, java.lang.Boolean.TRUE)
    bootstrap.handler(new ChannelInitializer[SocketChannel] {
      override def initChannel(ch: SocketChannel): Unit = {
        val pipeline = ch.pipeline()
        if (isSSLEnabled) {
          pipeline.addLast(sslClientCtx.newHandler(ch.alloc(), inet.getHostName(), inet.getPort()))
        } else {
          pipeline.addLast(
            new LengthFieldBasedFrameDecoder(Short.MaxValue, 0, 2, 0, 2),
            new LengthFieldPrepender(2)
          )
        }
        pipeline.addLast(new TCPPacketClientHandler(TcpRuntime.this, group.self, getAddress, replica))
      }
    })
    bootstrap.connect(inet).addListener(new ChannelFutureListener() {
      override def operationComplete(future: ChannelFuture): Unit = {
        if (future.cause() != null) {
          Logger("TcpRuntime", Warning, "Couldn't connect, trying again...")
          future.channel.close
          delayedStartConnection(replica)
        }
      }
    })
  }

  def startServer: Unit = {
    startListener()
    group.others.foreach { recipient =>
      if (directory.self.id < recipient.id.id)
        startConnection(recipient)
    }
    Thread.sleep(1000)
  }

  protected[psync] def send(to: ProcessID, buf: ByteBuf): Unit = {
    val chanOption = recipientMap.get(to)
    chanOption match {
      case Some(chan) =>
        chan.writeAndFlush(buf, chan.voidPromise())
      case None =>
        Logger("TcpRuntime", Warning, "Tried to send packet to " + to.id + ", but no channel was available.")
    }
  }

}

abstract class TCPPacketHandler(val packetServer: TcpRuntime, var selfId: ProcessID, var remote: Replica) extends SimpleChannelInboundHandler[ByteBuf](false) {

  def rename(newGroup: Group): Unit = {
    selfId = newGroup.self
    if (remote != null) {
      newGroup.getSafe(remote.netAddress) match {
        case Some(r) =>
          remote = r
        case None =>
          remote = new Replica(new ProcessID(-1), remote.address, remote.port)
      }
    }
  }

  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    Logger("TCPPacketHandler", Debug, "Someone connected to " + selfId.id)
    packetServer.addHandler(this)
  }

  override def channelInactive(ctx: ChannelHandlerContext): Unit = {
    if (remote != null) {
      if (remote.id != new ProcessID(-1)) {
        packetServer.removeChannel(remote.id)
        Logger("TCPPacketHandler", Debug, remote.id.id.toString + " disconnected")
      } else {
        packetServer.removeUnknownChannel(remote.netAddress)
        Logger("TCPPacketHandler", Debug, "unknown " + remote + " disconnected")
      }
    } else {
      Logger("TCPPacketHandler", Debug, "Someone disconnected")
    }
    packetServer.removeHandler(this)
  }

  override def channelRead0(ctx: ChannelHandlerContext, buf: ByteBuf): Unit = {
    val msg = new Message(selfId, remote.id, buf)
    try {
      if (remote.id == new ProcessID(-1)) {
         Logger("TCPPacketServerHandler", Warning, "dropping message from unknown (" + remote + ")")
         msg.release
      } else {
        packetServer.dispatch(msg)
      }
    } catch {
      case t: Throwable =>
        Logger("TCPPacketHandler", Warning, "got " + t + "\n  " + t.getStackTrace.mkString("\n  "))
    }
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    cause.printStackTrace()
  }

}

class TCPPacketServerHandler(
    _packetServer: TcpRuntime,
    _selfId: ProcessID,
    acceptUnknownConnection: Boolean
  ) extends TCPPacketHandler(_packetServer, _selfId, null) {

  protected def readAddress(buf: ByteBuf): Option[InetSocketAddress] = {
    val size = buf.readInt
    val string = buf.readCharSequence(size, UTF_8).toString
    buf.release
    Logger("TCPPacketServerHandler", Debug, "readAddress: " + string)
    val split = string.lastIndexOf(':')
    if (split < 0) {
      None
    } else {
      try {
        val start = if (string(0) == '/') 1 else 0
        val host = string.substring(start, split)
        val port = string.substring(split + 1).toInt
        val address = new InetSocketAddress(host, port)
        Some(address)
      } catch {
        case t: Throwable =>
          None
      }
    }
  }

  override def channelRead0(ctx: ChannelHandlerContext, buf: ByteBuf): Unit = {
    if (remote == null) {
      // First message is an InetSocketAddress
      readAddress(buf) match {
        case Some(addr) =>
          Logger("TCPPacketServerHandler", Info, addr.toString + " is connecting to " + selfId.id)
          val remoteAddress = addr
          packetServer.group.getSafe(remoteAddress) match {
            case Some(replica) =>
              packetServer.addChannel(replica.id, ctx.channel())
              Logger("TCPPacketServerHandler", Info, "<- Client " + replica.id + " connected to Server " + selfId.id)
              remote = replica
            case None =>
              if (acceptUnknownConnection) {
                packetServer.addUnknownChannel(remoteAddress, ctx.channel())
                Logger("TCPPacketServerHandler", Info, "Client " + remoteAddress + " connected to Server " + selfId.id)
                remote = new Replica(new ProcessID(-1), addr.getHostName, addr.getPort)
              } else {
                ctx.close()
                Logger("TCPPacketServerHandler", Info, "unknown connection " + remoteAddress + " closing.")
              }
          }
        case None =>
          Logger("TCPPacketServerHandler", Warning, "failed to parse/resolve connecting address, closing.")
          ctx.close()
      }
    } else {
      super.channelRead0(ctx, buf)
    }
  }

}

class TCPPacketClientHandler(
    _packetServer: TcpRuntime,
    _selfId: ProcessID,
    localAddress: InetSocketAddress,
    _remote: Replica
  ) extends TCPPacketHandler(_packetServer, _selfId, _remote) {

  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    super.channelActive(ctx)
    Logger("TCPPacketClientHandler", Debug, "connecting from " + localAddress + " to " + remote)
    val payload = ctx.alloc().buffer()
    payload.writeInt(-1)
    val length = payload.writeCharSequence(localAddress.toString, UTF_8)
    payload.setInt(0, length)
    val chan = ctx.channel()
    chan.writeAndFlush(payload, chan.voidPromise())
    packetServer.addChannel(remote.id, chan)
    Logger("TCPPacketClientHandler", Info, "-> Client " + selfId.id + " connected to " + remote.id.id)
  }

  override def channelInactive(ctx: ChannelHandlerContext): Unit = {
    super.channelInactive(ctx)
    packetServer.delayedStartConnection(remote)
  }

}

