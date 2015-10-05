package example

import psync._
import psync.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.net.InetSocketAddress
import io.netty.bootstrap._
import io.netty.buffer._
import io.netty.channel._
import io.netty.channel.nio._
import io.netty.channel.socket._
import io.netty.channel.socket.nio._
import io.netty.util.CharsetUtil
import java.util.concurrent.Semaphore


class LockManager(self: Short,
                  clientPort: Int) {
 
  type ProcessID = Short

  ////////////////
  //local state //
  ////////////////
  @volatile
  private var locked: Option[ProcessID] = None //who has the lock
  @volatile
  private var versionNbr = 0

  ///////////////
  // Consensus //
  ///////////////
  
  private val semaphore = new Semaphore(1, true) //at most one consensus at the time

  private val consensus: Runtime[ConsensusIO,_] = {
    if (Main.lv) new Runtime(new LastVoting, Main, defaultHandler(_))
    else new Runtime(new OTR, Main, defaultHandler(_))
  }

  private def onDecideOther(decision: Option[ProcessID]) {
    locked = decision
    versionNbr += 1 
    semaphore.release
  }

  private def onDecideSelf(client: Client, decision: Option[ProcessID]) {
    onDecideOther(decision)
    client.reply(decision)
  }

  //TODO we can get some deadlock here!
  private def startConsensus(expectedInstance: Short, io: ConsensusIO, msgs: Set[Message] = Set.empty) {
    Logger("LockManager", Notice, "starting consensus with value " + io.initialValue)
    //enter critical section
    semaphore.acquire
    //check instanceNbr
    if (expectedInstance == versionNbr + 1) {
      //make the paramerter and start the instance
      consensus.startInstance(expectedInstance, io, msgs)
    } else if (expectedInstance <= versionNbr){
      //msg.sender is late
      //or race on receiving msg and the default handler (message was blocked in the pipeline)
      //our implementation should exclude the later case ...
      msgs.foreach(_.release)
      semaphore.release
    } else { //if (expectedInstance > versionNbr+1){
      //we are late
      //start recovery ?
      msgs.foreach(_.release)
      semaphore.release
    }
  }

  //////////////////////////
  // setup, shutdown, ... //
  //////////////////////////

  def defaultHandler(msg: Message) = {

    //get the initial value from the msg (to avoid defaulting on -1)
    val content: Int = {
      if (Main.lv) {
        import scala.pickling._ 
        import binary._ 
        msg.round % 4 match {
          case 0 => msg.getInt(0)//getContent[(Int,Int)]._1
          case 1 => msg.getInt(0)
          case 2 => -1
          case 3 => msg.getInt(0)
          case _ => sys.error("???")
        }
      } else {
        msg.getInt(0)
      }
    }

    val io = new ConsensusIO {
      val initialValue = content
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
    consensus.startService
    listenForClient //this never returns
  }

  //clean-up on ctrl-c
  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() { shutDown }
    }
  )

  ////////////
  // Client //
  ////////////

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
      Logger("LockManager", Notice, "reply to " + success + " to client " + address)
    }

  }

  private class ClientHandler extends SimpleChannelInboundHandler[DatagramPacket] {
    //in Netty version 5.0 will be called: channelRead0 will be messageReceived
    override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
      val request = pkt.content().toString(CharsetUtil.UTF_8).toLowerCase
      val sender = pkt.sender()
      Logger("LockManager", Notice, "new client request " + request + " from " + sender)
      val initValue = request match {
        case `reqAcquire` => self.toInt
        case `reqRelease` => -1
        case _ => sys.error("unnkown request")
      }
      val client = new Client(sender, initValue != -1)
      val io = new ConsensusIO {
        val initialValue = initValue
        def decide(value: Int) {
          val dec = if (value == -1) None else Some(value.toShort)
          onDecideSelf(client, dec)
        }
      }
      if ((request == reqAcquire && locked.isEmpty) || 
          (request == reqRelease && locked.isDefined && locked.get == self)) {
        startConsensus((versionNbr+1).toShort, io)
      } else {
        client.reply(None) //fail
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

      Logger("LockManager", Notice, "listening for client on " + clientPort)
      clientChannel = b.bind(clientPort).sync().channel()
      clientChannel.closeFuture().await();
    } finally {
      group.shutdownGracefully()
    }
  }


}


class LockManagerClient(myPort: Int, remote: (String, Int)) {

  val address = new InetSocketAddress(remote._1, remote._2)

  private var critical = false

  def nextRequest = if (critical) "release" else "acquire"

  private class ReplyHandler extends SimpleChannelInboundHandler[DatagramPacket] {
    //in Netty version 5.0 will be called: channelRead0 will be messageReceived
    override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
      val reply = pkt.content().toString(CharsetUtil.UTF_8).toLowerCase
      Logger("LockManagerClient", Notice, "request: " + reply)
      if (reply == "success") {
        critical = !critical
      }
    }

  }
    
  def run {
    val group = new NioEventLoopGroup(1);
    try {
      val b = new Bootstrap();
      b.group(group)
        .channel(classOf[NioDatagramChannel])
        .handler(new ReplyHandler)

      Logger("LockManagerClient", Notice, "binding to " + myPort)
      val channel = b.bind(myPort).sync().channel()
      try {  
        var input = scala.io.StdIn.readLine()
        while (input != "exit") {
          val req = nextRequest
          Logger("LockManagerClient", Notice, "new request: " + req)
          val pck = new DatagramPacket(Unpooled.copiedBuffer(req, CharsetUtil.UTF_8), address)
          channel.writeAndFlush(pck).sync()
          input = scala.io.StdIn.readLine()
        }
      } finally {
        Logger("LockManagerClient", Notice, "shutting down")
        channel.close
      }
    } finally {
      group.shutdownGracefully()
    }
  }

}

object Main extends RTOptions {
  import dzufferey.arg._

  var client = false
  newOption("-c", Unit(() => client = true), "client mode (default is server mode)")

  var clientPort = 8889
  newOption("-p", Int( i => clientPort = i), "port")
  var remotePort = 8888
  newOption("-rp", Int( i => remotePort = i), "remote port")
  var remoteAddress = "127.0.0.1"
  newOption("-ra", String( str => remoteAddress = str), "replica address")

  var lv = false
  newOption("-lv", Unit( () => lv = true), "use the last voting instead of the OTR")

  var confFile = "src/test/resources/sample-conf.xml"

  val usage = "..."

  def main(args: Array[java.lang.String]) {
    Logger.moreVerbose
    val args2 = if (args contains "--conf") args else "--conf" +: confFile +: args
    apply(args2)
    if (client) {
      val cli = new LockManagerClient(clientPort, (remoteAddress, remotePort))
      cli.run
    } else {
      val srv = new LockManager(id.toShort, clientPort)
      srv.start
    }
  }

}
