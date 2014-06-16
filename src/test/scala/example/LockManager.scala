package example

import round._
import round.runtime._
import java.net.InetSocketAddress
import io.netty.bootstrap._
import io.netty.buffer._
import io.netty.channel._
import io.netty.channel.nio._
import io.netty.channel.socket._
import io.netty.channel.socket.nio._
import io.netty.util.CharsetUtil



class LockManager(self: Short) {
 
  type ProcessID = Short

  ////////////////
  //local state //
  ////////////////

  @volatile
  private var locked: Option[ProcessID] = None //who has the lock
  @volatile
  private var versionNbr = 0 //consensus finished
  @volatile
  private var runningNbr = 0 //consensus that is still running


  ////////////
  // Client //
  ////////////

  private val clientPort = 8888
  private var clientChannel: Channel = null
  private val reqAcquire = "acquire"
  private val reqRelease = "release"

  //TODO client over TCP ?
  private class Client(val address: InetSocketAddress, val acquire: Boolean) {
    def reply(status: Option[ProcessID]) {
      val success = (status.isEmpty && !acquire) ||
                    (status.isDefined && acquire && status.get == self)
      val message = if (success) "SUCCESS" else "FAILED"
      val pck = new DatagramPacket(Unpooled.copiedBuffer(message, CharsetUtil.UTF_8), address)
      clientChannel.writeAndFlush(pck).sync()
    }

  }

  private class ClientHandler extends SimpleChannelInboundHandler[DatagramPacket] {
    //in Netty version 5.0 will be called: channelRead0 will be messageReceived
    override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
      val request = pkt.content().toString(CharsetUtil.UTF_8).toLowerCase
      val sender = pkt.sender()
      pkt.release
      if (request == reqAcquire) {
        val client = new Client(sender, true)
        if (locked.isEmpty) {
          //start consensus
          val io = new OtrIO {
            val initialValue = self.toInt
            def decide(value: Int) {
              val dec = if (value == -1) None else Some(value.toShort)
              onDecideSelf(client)(dec)
            }
          }
          startConsensus((runningNbr+1).toShort, io)
        } else {
          client.reply(None) //fail, lock already acquired
        }
      } else if (request == reqRelease) {
        val client = new Client(sender, false)
        if (locked.isDefined && locked.get == self) { //TODO check the client is the right one
          //TODO start consensus
          val io = new OtrIO {
            val initialValue = -1
            def decide(value: Int) {
              val dec = if (value == -1) None else Some(value.toShort)
              onDecideSelf(client)(dec)
            }
          }
          startConsensus((runningNbr+1).toShort, io)
        } else {
          client.reply(None) //fail, do not own the lock
        }
      } else {
        //otherwise drop the request
      }
    }

  }

  def listenForClient {
    val group = new NioEventLoopGroup();
    try {
      val b = new Bootstrap();
      b.group(group)
        .channel(classOf[NioDatagramChannel])
        .handler(new ClientHandler)

      clientChannel = b.bind(clientPort).sync().channel()
      clientChannel.closeFuture().await();
    } finally {
      group.shutdownGracefully()
    }
  }


  ///////////////
  // Consensus //
  ///////////////

  //TODO a queue of request, ...

  private val algorithm = new OTR
  private val consensus = new RunTime(algorithm)

  private def onDecideOther(decision: Option[ProcessID]) {
    locked = decision
    versionNbr = runningNbr
  }

  private def onDecideSelf(client: Client)(decision: Option[ProcessID]) {
    locked = decision
    versionNbr = runningNbr
    client.reply(decision)
  }

  private def startConsensus(expectedInstance: Short, io: OtrIO, msgs: Set[Message[ByteBuf]] = Set.empty) {
    //check instanceNbr
    if (expectedInstance == runningNbr+1 && runningNbr == versionNbr) {
      //make the paramerter and start the instance
      runningNbr == expectedInstance
      consensus.startInstance(expectedInstance, io, msgs)
    } else if (expectedInstance == runningNbr + 1){
      //starting the next instance, while the current one is not yet terminated
    } else if (expectedInstance == runningNbr){
      //race on receiving msg and the default handler
    } else if (expectedInstance < runningNbr){
      //msg.sender is late
    } else if (expectedInstance > runningNbr+1){
      //we are late
    } else {
      //something is wrong (need to look in the message tag)
    }
  }

  //////////////////////////
  // setup, shutdown, ... //
  //////////////////////////

  def defaultHandler(msg: Message[ByteBuf]) = {
    val io = new OtrIO {
      val initialValue = -1
      def decide(value: Int) {
        if (value == -1) onDecideOther(None)
        else onDecideOther(Some(value.toShort))
      }
    }
    startConsensus(msg.instance, io, Set(msg))
  }

  def shutDown {
    consensus.shutdown
    clientChannel.close
  }

  def start() {
    consensus.startService(defaultHandler)
    listenForClient //this never returns
  }

  //clean-up on ctrl-c
  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() { shutDown }
    }
  )

}
