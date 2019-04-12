package psync.runtime

import psync._
import io.netty.buffer.{ByteBuf,PooledByteBufAllocator}
import io.netty.channel.socket._
import dzufferey.utils.LogLevel._
import dzufferey.utils.Logger
import java.util.concurrent.{Executors, ExecutorService}
import io.netty.channel.EventLoopGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.epoll.EpollEventLoopGroup


abstract class Runtime(val options: RuntimeOptions, defaultHandler: Message => Unit) {

  protected val dispatcher = new InstanceDispatcher(options)
  protected var executor: ExecutorService = null
  protected val directory = {
    val me = new ProcessID(options.id)
    val grp = Group(me, options.peers)
    new Directory(grp)
  }

  def group = directory.group
  /** The group should not be changed while instances are running */
  def group_=(grp: Group) {
    directory.group = grp
  }
    
  protected def port = {
    if (directory contains directory.self) directory.get(directory.self).port
    else options.port
  }

  protected def evtGroup: EventLoopGroup = {
    assert(executor != null)
    options.group match {
      case NetworkGroup.NIO => new NioEventLoopGroup(0, executor)
      case NetworkGroup.EPOLL => new EpollEventLoopGroup(0, executor)
    }
  }
  
  protected def createExecutor = {
    options.workers match {
      case Factor(n) =>
        val w = n * java.lang.Runtime.getRuntime().availableProcessors()
        Logger("Runtime", Debug, "using fixed thread pool of size " + w)
        Executors.newFixedThreadPool(w)
      case Fixed(n) =>
        val w = n
        Logger("Runtime", Debug, "using fixed thread pool of size " + w)
        Executors.newFixedThreadPool(w)
      case Adapt =>
        Logger("Runtime", Debug, "using cached thread pool")
        Executors.newCachedThreadPool()
    }
  }

  /** Start an instance of the algorithm. */
  protected[psync] def startInstance[IO, P <: Process[IO]](
      instanceId: Short,
      process: InstanceHandler[IO,P],
      io: IO,
      messages: Set[Message] = Set.empty)
  {
    Logger("Runtime", Info, "starting instance " + instanceId)
    if (executor != null) {
      //an instance is actually encapsulated by one process
      val messages2 = messages.filter( m => {
        if (!Flags.userDefinable(m.flag) && m.flag != Flags.dummy) {
          true
        } else {
          m.release
          false
        }
      })
      process.prepare(io, group, instanceId, messages2)
      dispatcher.add(instanceId, process)
      submitTask(process)
    } else {
      Logger.logAndThrow("Runtime", Error, "service not running")
    }
  }

  /** Stop a running instance of the algorithm. */
  protected[psync] def stopInstance(instanceId: Short) {
    Logger("Runtime", Info, "stopping instance " + instanceId)
    if (executor != null) {
      dispatcher.findInstance(instanceId).map(_.interrupt(instanceId))
    } else {
      Logger.logAndThrow("Runtime", Error, "service not running")
    }
  }
  
  protected[psync] def remove(instanceId: Short) {
    dispatcher.remove(instanceId)
  }
  
  protected[psync] def default(msg: Message) {
    submitTask(new Runnable { def run = defaultHandler(msg) })
  }

  /** Start the service that ... */
  def startService {
    if (executor != null) {
      //already running
      return
    }

    //create the group
    executor = createExecutor
    Logger("Runtime", Info, "replica " + directory.self.id + " has group:\n  " + group.asList.mkString("\n  "))
    Logger("Runtime", Info, "starting service on port: " + port)
    startServer
  }

  def shutdown {
    if (executor != null) {
      Logger("Runtime", Info, "stopping service")
      closeServer
      executor.shutdownNow
      executor = null
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
    assert(executor != null)
    payload.setLong(0, tag.underlying)
    send(dest, payload)
  }

  /* Try to deliver a message.
   * Useful when multiple defaulthandlers interleave */
  def deliverMessage(m: Message): Boolean = {
    dispatcher.dispatch(m)
  }
  
  protected[psync] def dispatch(msg: Message) {
    if (Flags.userDefinable(msg.flag) || !dispatcher.dispatch(msg)) {
      defaultHandler(msg)
    }
  }

  /*****************/
  /* abstract part */
  /*****************/

  protected def closeServer: Unit
  protected def startServer: Unit
  protected[psync] def send(to: ProcessID, buf: ByteBuf)

}

object Runtime {

  def apply(options: RuntimeOptions, defaultHandler: Message => Unit): Runtime = {
    options.protocol match {
      case NetworkProtocol.UDP =>
        new UdpRuntime(options, defaultHandler)
      case _ =>
        new TcpRuntime(options, defaultHandler)
    }
  }

}
