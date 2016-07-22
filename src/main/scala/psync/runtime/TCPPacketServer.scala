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
import java.net.InetSocketAddress
import java.util.concurrent.{Executors, TimeUnit}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{Map, SynchronizedMap, HashMap}

class TCPPacketServer(
    executor: java.util.concurrent.Executor,
    port: Int,
    val initGroup: Group,
    _defaultHandler: Message => Unit, //defaultHandler is responsible for releasing the ByteBuf payload
    options: RuntimeOptions) extends PacketServer(executor, port, initGroup, _defaultHandler, options)
{

  val recipientMap: Map[Short, Channel] = new TrieMap[Short, Channel]// with SynchronizedMap[ProcessID, Channel]
  private val recipients: Set[ProcessID] = initGroup.replicas.map(_.id).toSet - initGroup.self

  def defaultHandler(pkt: DatagramPacket) {
    val msg = new Message(pkt, directory.group)
    _defaultHandler(msg)
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

  private val group: EventLoopGroup = new NioEventLoopGroup()

  private val connectionRestartPeriod = 250

  def close {
    dispatcher.clear
    try {
      group.shutdownGracefully
    } finally {
      recipientMap.foreach {
        case (pid, channel) =>
          channel.close()
      }
    }
  }

  val outerThis = this

  def startListener() {
    val bootstrap = new ServerBootstrap()
    bootstrap.group(group)
    bootstrap.channel(classOf[NioServerSocketChannel])
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
        pipeline.addLast(new TCPPacketServerHandler(outerThis))
      }
    })
    bootstrap.bind(port).sync()
  }

  def delayedStartConnction(id: ProcessID) {
    if (group.isShuttingDown())
      return
    val loop = group.next()
    loop.schedule(new Runnable() {
      override def run() {
        startConnection(id)
      }
    }, connectionRestartPeriod, TimeUnit.MILLISECONDS)
  }

  def startConnection(id: ProcessID) {
    val bootstrap = new Bootstrap()
    val inet = initGroup.idToInet(id)
    bootstrap.group(group)
    bootstrap.channel(classOf[NioSocketChannel])
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
        pipeline.addLast(new TCPPacketClientHandler(outerThis, initGroup.self, id))
      }
    })
    bootstrap.connect(inet).addListener(new ChannelFutureListener() {
      override def operationComplete(future: ChannelFuture) {
        if (future.cause() != null) {
          Logger("TCPPacketServer", Warning, "Couldn't connect, trying again...")
          delayedStartConnction(id)
        }
      }
    })
  }

  def start {
    startListener()
    recipients.foreach { recipient =>
      if (initGroup.self.id < recipient.id)
        startConnection(recipient)
    }
    Thread.sleep(1000)
  }

  def send(pkt: DatagramPacket) {
    val recipientAddress = pkt.recipient
    val recipientID = initGroup.get(recipientAddress).id
    val chanOption = recipientMap.get(recipientID.id)
    chanOption match {
      case Some(chan) =>
        chan.write(pkt.content, chan.voidPromise())
        chan.flush
      case None =>
        Logger("TCPPacketServer", Info, "Tried to send packet, but no channel was available.")
        // Todo: have some sort of queue that waits for the client to come back up?
        //       Drop the packet?
    }
  }

  def receive(process: ProcessID, buf: ByteBuf) {
    val inet = initGroup.idToInet(process)
    val src = initGroup.idToInet(initGroup.self)
    val pkt = new DatagramPacket(buf, src, inet)
    try {
      if (!dispatcher.dispatch(pkt))
        defaultHandler(pkt)
    } catch {
      case t: Throwable =>
        Logger("TCPPacketServerHandler", Warning, "got " + t)
    }
  }

}

class TCPPacketServerHandler(
    packetServer: TCPPacketServer
  ) extends SimpleChannelInboundHandler[ByteBuf](false) {

  var processID: Option[ProcessID] = None

  override def channelActive(ctx: ChannelHandlerContext) {
    Logger("TCPPacketServerHandler", Debug, "Someone connected.")
  }

  override def channelInactive(ctx: ChannelHandlerContext) {
    Logger("TCPPacketServerHandler", Debug, "Someone disconneted")
    if (processID.isDefined)
      packetServer.recipientMap.remove(processID.get.id)
  }

  override def channelRead0(ctx: ChannelHandlerContext, buf: ByteBuf) {
    if (processID.isEmpty) {
      // First message is always a processID
      processID = Some(new ProcessID(buf.getShort(0)))
      buf.release
      packetServer.recipientMap.update(processID.get.id, ctx.channel())
      Logger("TCPPacketServerHandler", Info, "Client " + processID.get.id + " connected to server " + packetServer.initGroup.self.id)
    } else {
      packetServer.receive(processID.get, buf)
    }
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
  }
}

class TCPPacketClientHandler(
    packetServer: TCPPacketServer,
    localID: ProcessID,
    remoteID: ProcessID
  ) extends SimpleChannelInboundHandler[ByteBuf](false) {

  override def channelRead0(ctx: ChannelHandlerContext, buf: ByteBuf) {
    packetServer.receive(remoteID, buf)
  }

  override def channelActive(ctx: ChannelHandlerContext) {
    Logger("TCPPacketClientHandler", Debug, "Someone connected.")
    val chan = ctx.channel()
    packetServer.recipientMap.update(remoteID.id, chan)
    val payload = PooledByteBufAllocator.DEFAULT.buffer()
    payload.writeShort(localID.id)
    chan.write(payload, chan.voidPromise())
    chan.flush
  }

  override def channelInactive(ctx: ChannelHandlerContext) {
    Logger("TCPPacketClientHandler", Debug, "Someone disconneted.")
    packetServer.recipientMap.remove(remoteID.id)
    packetServer.delayedStartConnction(remoteID)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
  }
}

