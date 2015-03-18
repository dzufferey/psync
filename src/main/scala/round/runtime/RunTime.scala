package round.runtime

import round._
import round.predicate._
import io.netty.buffer.ByteBuf
import io.netty.channel.socket._
import dzufferey.utils.LogLevel._
import dzufferey.utils.Logger
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.AtomicInteger


class RunTime[IO](val alg: Algorithm[IO]) {

  private var srv: Option[PacketServer] = None

  private var options = Map.empty[String, String]

  private val defaultSize = 64
  private val maxSize = {
    try {
      options.getOrElse("processPool", defaultSize.toString).toInt
    } catch { case e: Exception =>
      Logger("RunTime", Warning, "processPool: wrong format, using " + defaultSize)
      defaultSize
    }
  }
  private val processPool = new ArrayBlockingQueue[InstanceHandler[IO]](maxSize)

  private var channelIdx = new AtomicInteger
  private def createProcess: InstanceHandler[IO] = {
    assert(srv.isDefined)
    val p = alg.process
    val channels = srv.get.channels
    val idx = channelIdx.getAndIncrement.abs % channels.size
    val channel = channels(idx)
    val dispatcher = srv.get.dispatcher
    val defaultHandler = srv.get.defaultHandler(_)
    new InstanceHandler(p, this, channel, dispatcher, defaultHandler, options)
  }

  private def getProcess: InstanceHandler[IO] = {
    val proc = processPool.poll
    if (proc == null) {
      Logger("RunTime", Warning, "processPool is running low")
      createProcess
    } else {
      proc
    }
  }

  def recycle(p: InstanceHandler[IO]) {
    processPool.offer(p)
  }

  /** Start an instance of the algorithm. */
  def startInstance(
      instanceId: Short,
      io: IO,
      messages: Set[Message] = Set.empty)
  {
    Logger("RunTime", Info, "starting instance " + instanceId)
    srv match {
      case Some(s) =>
        //an instance is actually encapsulated by one process
        val grp = s.directory.group
        val process = getProcess
        val messages2 = messages.filter( m => {
          if (!Flags.userDefinable(m.flag) && m.flag != Flags.dummy) {
            true
          } else {
            m.release
            false
          }
        })
        process.prepare(io, grp, instanceId, messages2)
        submitTask(process)
      case None =>
        sys.error("service not running")
    }
  }

  /** Stop a running instance of the algorithm. */
  def stopInstance(instanceId: Short) {
    Logger("RunTime", Info, "stopping instance " + instanceId)
    srv match {
      case Some(s) =>
        s.dispatcher.findInstance(instanceId).map(_.interrupt(instanceId))
      case None =>
        sys.error("service not running")
    }
  }

  /** Start the service that ... */
  def startService(
    defaultHandler: Message => Unit,
    peers: List[Replica],
    options: Map[String, String]
  ) {
    if (srv.isDefined) {
      //already running
      return
    }
    
    //create the group
    val me = new ProcessID(options("id").toShort)
    val grp = Group(me, peers)

    //start the server
    val ports = 
      if (grp contains me) grp.get(me).ports
      else Set(options("port").toInt)
    Logger("RunTime", Info, "starting service on ports: " + ports.mkString(", "))
    val pktSrv = new PacketServer(ports, grp, defaultHandler, options)
    srv = Some(pktSrv)
    pktSrv.start
    for (i <- 0 until maxSize) processPool.offer(createProcess)
  }

  def startService(
    defaultHandler: Message => Unit,
    configFile: String,
    additionalOpt: Map[String, String]
  ) {

    //parse config
    val (peers,param1) = Config.parse(configFile)
    options = param1 ++ additionalOpt

    startService(defaultHandler, peers, options)    
  }

  def shutdown {
    srv match {
      case Some(s) =>
        Logger("RunTime", Info, "stopping service")
        s.close
      case None =>
    }
    srv = None
  }
  
  def submitTask[T](fct: java.util.concurrent.Callable[T]) = {
    assert(srv.isDefined)
    srv.get.submitTask(fct)
  }


  def submitTask[T](fct: () => T) = {
    assert(srv.isDefined)
    srv.get.submitTask(new java.util.concurrent.Callable[T]{
      def call: T = fct()
    })
  }

  /** the first 8 bytes of the payload must be empty */
  def sendMessage(dest: ProcessID, tag: Tag, payload: ByteBuf) = {
    assert(Flags.userDefinable(tag.flag) || tag.flag == Flags.dummy) //TODO in the long term, we might want to remove the dummy
    assert(srv.isDefined)
    val grp = srv.get.directory
    val dst = grp.idToInet(dest, tag.instanceNbr)
    payload.setLong(0, tag.underlying)
    val pkt =
      if (grp.contains(grp.self)) {
        val src = grp.idToInet(grp.self)
        new DatagramPacket(payload, dst, src)
      } else {
        new DatagramPacket(payload, dst)
      }
    val channel = srv.get.channels(0)
    channel.write(pkt, channel.voidPromise())
    channel.flush
  }

  def directory = {
    assert(srv.isDefined)
    srv.get.directory
  }

}
