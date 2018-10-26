package psync.runtime

import psync._
import io.netty.channel.Channel
import io.netty.channel.socket.DatagramPacket
import io.netty.buffer.ByteBuf
import io.netty.channel.EventLoopGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.epoll.EpollEventLoopGroup
import io.netty.channel.oio.OioEventLoopGroup

abstract class PacketServer(
    executor: java.util.concurrent.Executor,
    port: Int,
    initGroup: Group,
    defaultHandler: Message => Unit, //defaultHandler is responsible for releasing the ByteBuf payload
    options: RuntimeOptions)
{

  protected val directory = new Directory(initGroup)

  def group = directory.group
  /** The group should not be changed while instances are running */
  def group_=(grp: Group) {
    directory.group = grp
  }

  protected def evtGroup: EventLoopGroup = options.group match {
    case NetworkGroup.NIO => new NioEventLoopGroup(0, executor)
    case NetworkGroup.OIO => new OioEventLoopGroup(0, executor)
    case NetworkGroup.EPOLL => new EpollEventLoopGroup(0, executor)
  }

  protected[psync] val dispatcher = new InstanceDispatcher(options)

  def close: Unit

  def start: Unit

  def send(to: ProcessID, buf: ByteBuf): Unit

  def dispatch(msg: Message) {
    if (Flags.userDefinable(msg.flag) || !dispatcher.dispatch(msg)) {
      defaultHandler(msg)
    }
  }

}
