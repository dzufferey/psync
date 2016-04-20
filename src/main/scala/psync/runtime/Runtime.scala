package psync.runtime

import psync._
import io.netty.buffer.ByteBuf
import io.netty.channel.socket._
import dzufferey.utils.LogLevel._
import dzufferey.utils.Logger
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.AtomicInteger


class Runtime[IO,P <: Process[IO]](val alg: Algorithm[IO,P],
                  options: RuntimeOptions,
                  defaultHandler: Message => Unit) {

  private var srv: Option[PacketServer] = None

  //TODO try a stack for better locality
  private val processPool = new ArrayBlockingQueue[InstanceHandler[IO,P]](options.processPool)

  private val executor = options.workers match {
    case Factor(n) =>
      val w = n * java.lang.Runtime.getRuntime().availableProcessors()
      Logger("Runtime", Debug, "using fixed thread pool of size " + w)
      java.util.concurrent.Executors.newFixedThreadPool(w)
    case Fixed(n) =>
      val w = n
      Logger("Runtime", Debug, "using fixed thread pool of size " + w)
      java.util.concurrent.Executors.newFixedThreadPool(w)
    case Adapt =>
      Logger("Runtime", Debug, "using cached thread pool")
      java.util.concurrent.Executors.newCachedThreadPool()
  }

  private def createProcess: InstanceHandler[IO,P] = {
    assert(srv.isDefined)
    val p = alg.process
    p.setOptions(options)
    val dispatcher = srv.get.dispatcher
    val defaultHandler = srv.get.defaultHandler(_)
    new InstanceHandler(p, this, srv.get, dispatcher, defaultHandler, options)
  }

  private def getProcess: InstanceHandler[IO,P] = {
    val proc = processPool.poll
    if (proc == null) {
      Logger("Runtime", Warning, "processPool is running low")
      createProcess
    } else {
      proc
    }
  }

  def recycle(p: InstanceHandler[IO,P]) {
    processPool.offer(p)
  }

  /** Start an instance of the algorithm. */
  def startInstance(
      instanceId: Short,
      io: IO,
      messages: Set[Message] = Set.empty)
  {
    Logger("Runtime", Info, "starting instance " + instanceId)
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
    Logger("Runtime", Info, "stopping instance " + instanceId)
    srv match {
      case Some(s) =>
        s.dispatcher.findInstance(instanceId).map(_.interrupt(instanceId))
      case None =>
        sys.error("service not running")
    }
  }

  /** Start the service that ... */
  def startService {
    if (srv.isDefined) {
      //already running
      return
    }

    //create the group
    val me = new ProcessID(options.id)
    val grp = Group(me, options.peers)

    //start the server
    val ports =
      if (grp contains me) grp.get(me).ports
      else Set(options.port)
    val port = ports.iterator.next()
    Logger("Runtime", Info, "starting service on ports: " + ports.mkString(", "))
    val pktSrv = options.protocol match {
      case NetworkProtocol.UDP =>
        new UDPPacketServer(executor, port, grp, defaultHandler, options)
      case _ =>
        new TCPPacketServer(executor, port, grp, defaultHandler, options)
    }
    srv = Some(pktSrv)
    pktSrv.start
    for (i <- 0 until options.processPool) processPool.offer(createProcess)
  }

  def shutdown {
    srv match {
      case Some(s) =>
        Logger("Runtime", Info, "stopping service")
        s.close
        executor.shutdownNow
      case None =>
    }
    srv = None
  }

  def submitTask(fct: Runnable) = {
    executor.execute(fct)
  }


  def submitTask(fct: () => Unit) = {
    executor.execute(new Runnable{
      def run = fct()
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
    srv.get.send(pkt)
  }

  def directory = {
    assert(srv.isDefined)
    srv.get.directory
  }

}
