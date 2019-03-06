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
import java.util.concurrent.locks.ReentrantLock
import psync.utils.serialization._
import com.esotericsoftware.kryo.Kryo

/* A simple example that shows how to use PSync algorithm to accomplish in a "larger" system.
 * However, the example is simpler than what it should be. For instance, it does not deal with
 * overflow in the running/version number, the client request should have unique id, etc.
 */

class LockManager(self: Short,
                  clientPort: Int) {
 
  final val Decision = 4

  ////////////////
  //local state //
  ////////////////
  @volatile
  private var locked: Option[ProcessID] = None //who has the lock
  @volatile
  private var versionNbr: Short = 0
  private var runningNbr: Short = 0

  ///////////////
  // Consensus //
  ///////////////
  
  private val lock = new ReentrantLock
  
  val kryo = new ThreadLocal[Kryo] {
    override def initialValue = regIntTimePair.register(KryoSerializer.serializer)
  }

  private val consensus: Runtime[ConsensusIO,_] = {
    if (Main.lv) new Runtime(new LastVotingEvent(Main.timeout), Main, defaultHandler(_))
    else new Runtime(new OTR, Main, defaultHandler(_))
  }

  private def onDecide(version: Short, decision: Option[ProcessID]) {
    lock.lock
    if (version > versionNbr) {
      locked = decision
      versionNbr = version
      pendingRequest.foreach{ case (client, v) =>
        if (v < version) {
          client.reply(None)
          pendingRequest = None
        } else if (v == version) {
          client.reply(decision)
          pendingRequest = None
        }
      }
      Logger("LockManager", Notice, "decision: " + locked + " @ " + versionNbr)
    }
    lock.unlock
  }

  private def recover(msg: Message) {
    lock.lock
    for(potentiallyRunning <- (versionNbr+1) to runningNbr) {
      consensus.stopInstance(potentiallyRunning.toShort)
    }
    val version = msg.instance
    runningNbr = math.max(runningNbr, version).toShort
    val state = msg.getContent[Int](kryo.get)
    val dec = if (state == -1) None else Some(new ProcessID(state.toShort))
    Logger("LockManager", Notice, "got recovery message " + state + " @ " + version)
    onDecide(version, dec)
    lock.unlock
  }

  private def sendRecoveryInfo(m: Message) = {
    val tag = Tag(versionNbr, 0, Decision, 0)
    val state: Int = if (locked == None) -1 else locked.get.id
    val payload = m.payload
    payload.clear
    payload.retain
    m.release
    Message.setContent[Int](kryo.get, tag, payload, state)
    val sender = m.sender
    Logger("LockManager", Notice, "sending decision " + state + " @ " + versionNbr + " to " + sender)
    consensus.sendMessage(sender, tag, payload)
  }

  private def askRecoveryInfo(m: Message) = {
    val tag = Tag(versionNbr, 0, Flags.dummy, 0)
    val payload = m.payload
    payload.clear
    payload.retain
    m.release
    Message.setContent[Int](kryo.get, tag, payload, 0)
    val sender = m.sender
    Logger("LockManager", Notice, "asking decision" + versionNbr + " to " + sender)
    consensus.sendMessage(sender, tag, payload)
  }

  private def startConsensus(client: Option[Client], expectedInstance: Short, io: ConsensusIO, msgs: Set[Message] = Set.empty) {
    //enter critical section
    lock.lock
    val nextInstance = (runningNbr + 1).toShort
    //check instanceNbr
    if (expectedInstance == nextInstance) {
      //make the paramerter and start the instance
      Logger("LockManager", Notice, "starting consensus " + expectedInstance + " with value " + io.initialValue)
      consensus.startInstance(expectedInstance, io, msgs)
      runningNbr = expectedInstance
      client.foreach( c => {
        assert(pendingRequest == None)
        pendingRequest = Some(c -> expectedInstance)
      })
    } else if (expectedInstance <= versionNbr){
      //msg.sender is late
      //or race on receiving msg and the default handler (message was blocked in the pipeline)
      //our implementation should exclude the later case ...
      msgs.foreach(sendRecoveryInfo)
    } else if (expectedInstance == runningNbr){
      //some funny interleaving happened but fine
      msgs.foreach(_.release)
    } else { //if (expectedInstance > versionNbr+1){
      Logger("LockManager", Warning, "we are late " + runningNbr + " but should be at " + expectedInstance)
      msgs.foreach(askRecoveryInfo)
      client.foreach( c => {
        c.reply(None)
      })
    }
    lock.unlock
  }

  //////////////////////////
  // setup, shutdown, ... //
  //////////////////////////

  def defaultHandler(msg: Message) = {
    Logger("LockManager", Debug, "defaultHandler: " + msg.tag)

    if (msg.flag == Decision) {
      recover(msg)
    } else {

      //get the initial value from the msg (to avoid defaulting on -1)
      val content: Int = {
        if (Main.lv) {
          msg.round % 4 match {
            case 0 => msg.getContent[(Int,Time)](kryo.get)._1
            case 1 | 2 | 3 => msg.getContent[Int](kryo.get)
            case _ => sys.error("???")
          }
        } else {
          msg.getContent[Int](kryo.get)
        }
      }
 
      val io = new ConsensusIO {
        val initialValue = content
        def decide(value: Int) {
          if (value == -1) onDecide(msg.instance, None)
          else onDecide(msg.instance, Some(new ProcessID(value.toShort)))
        }
      }
 
      startConsensus(None, msg.instance, io, Set(msg))
    }
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
  
  private class Client(val address: InetSocketAddress, val acquire: Boolean) {
    def reply(status: Option[ProcessID]) {
      val success = (status.isEmpty && !acquire) ||
                    (status.isDefined && acquire && status.get.id == self)
      val message = if (success) "SUCCESS" else "FAILED"
      val pck = new DatagramPacket(Unpooled.copiedBuffer(message, CharsetUtil.UTF_8), address)
      clientChannel.writeAndFlush(pck).sync()
      Logger("LockManager", Notice, "reply to " + success + " to client " + address)
    }
  }

  private var clientChannel: Channel = null
  private val reqAcquire = "acquire"
  private val reqRelease = "release"
  private var pendingRequest: Option[(Client,Short)] = None

  private class ClientHandler extends SimpleChannelInboundHandler[DatagramPacket] {
    //in Netty version 5.0 will be called: channelRead0 will be messageReceived
    override def channelRead0(ctx: ChannelHandlerContext, pkt: DatagramPacket) {
      val request = pkt.content().toString(CharsetUtil.UTF_8).toLowerCase
      val sender = pkt.sender()
      val inst = (runningNbr+1).toShort
      Logger("LockManager", Notice, "new client request " + request + " from " + sender)
      val initValue = request match {
        case `reqAcquire` => self.toInt
        case `reqRelease` => -1
        case _ => sys.error("unkown request")
      }
      val client = new Client(sender, initValue != -1)
      val io = new ConsensusIO {
        val initialValue = initValue
        def decide(value: Int) {
          val dec = if (value == -1) None else Some(new ProcessID(value.toShort))
          onDecide(inst, dec)
        }
      }
      if (pendingRequest != None) {
        client.reply(None) //at most one pending request -> fail
      } else if ((request == reqAcquire && locked.isEmpty) || 
                 (request == reqRelease && locked.isDefined && locked.get.id == self)) {
        startConsensus(Some(client), inst, io)
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
      val srv = new LockManager(id, clientPort)
      srv.start
    }
  }

}
