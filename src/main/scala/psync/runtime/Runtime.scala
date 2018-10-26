package psync.runtime

import psync._
import io.netty.buffer.{ByteBuf,PooledByteBufAllocator}
import io.netty.channel.socket._
import dzufferey.utils.LogLevel._
import dzufferey.utils.Logger
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.atomic.AtomicInteger


class Runtime[IO,P <: Process[IO]](val alg: Algorithm[IO,P],
                  options: RuntimeOptions,
                  defaultHandler: Message => Unit) {

  private var srv: PacketServer = null

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
    assert(srv != null)
    val p = alg.process
    val dispatcher = srv.dispatcher
    new InstanceHandler(p, this, srv, dispatcher, defaultHandler, options)
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
    if (srv != null) {
      //an instance is actually encapsulated by one process
      val grp = srv.group
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
    } else {
      Logger.logAndThrow("Runtime", Error, "service not running")
    }
  }

  /** Stop a running instance of the algorithm. */
  def stopInstance(instanceId: Short) {
    Logger("Runtime", Info, "stopping instance " + instanceId)
    if (srv != null) {
      srv.dispatcher.findInstance(instanceId).map(_.interrupt(instanceId))
    } else {
      Logger.logAndThrow("Runtime", Error, "service not running")
    }
  }

  /** Start the service that ... */
  def startService {
    if (srv != null) {
      //already running
      return
    }

    //create the group
    val me = new ProcessID(options.id)
    val grp = Group(me, options.peers)
    Logger("Runtime", Info, "replica " + me.id + " has group:\n  " + grp.asList.mkString("\n  "))

    //start the server
    val port =
      if (grp contains me) grp.get(me).port
      else options.port
    Logger("Runtime", Info, "starting service on port: " + port)
    srv = options.protocol match {
      case NetworkProtocol.UDP =>
        new UDPPacketServer(executor, port, grp, defaultHandler, options)
      case _ =>
        new TCPPacketServer(executor, port, grp, defaultHandler, options)
    }
    srv.start
    for (i <- 0 until options.processPool) processPool.offer(createProcess)
  }

  def shutdown {
    if (srv != null) {
      Logger("Runtime", Info, "stopping service")
      srv.close
      executor.shutdownNow
      srv = null
    }
  }

  def submitTask(fct: Runnable) = {
    executor.execute(fct)
  }


  def submitTask(fct: () => Unit) = {
    executor.execute(new Runnable{
      def run = fct()
    })
  }

  /** Send an out-of-band message to another process.
   *  The first 8 bytes of the payload must be empty */
  def sendMessage(dest: ProcessID, tag: Tag, payload: ByteBuf) = {
    assert(Flags.userDefinable(tag.flag) || tag.flag == Flags.dummy) //TODO in the long term, we might want to remove the dummy
    assert(srv != null)
    payload.setLong(0, tag.underlying)
    srv.send(dest, payload)
  }

  def getGroup = {
    assert(srv != null)
    srv.group
  }

  /** BEWARE it is only safe to call this method when no instance is running! */
  def updateGroup(grp: Group) {
    assert(srv != null)
    srv.group = grp
  }

  /* Try to deliver a message.
   * Useful when multiple defaulthandlers interleave */
  def deliverMessage(m: Message): Boolean = {
    srv.dispatcher.dispatch(m)
  }

}
