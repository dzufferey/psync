package psync.runtime

import psync._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import io.netty.buffer._
import io.netty.channel._
import io.netty.channel.socket._
import io.netty.channel.nio._
import io.netty.channel.socket.nio._
import io.netty.channel.epoll._
import io.netty.channel.oio._
import io.netty.channel.socket.oio._
import io.netty.channel.ChannelHandler.Sharable
import io.netty.bootstrap.Bootstrap
import java.net.InetSocketAddress

abstract class PacketServer(
    executor: java.util.concurrent.Executor,
    ports: Iterable[Int],
    initGroup: Group,
    _defaultHandler: Message => Unit, //defaultHandler is responsible for releasing the ByteBuf payload
    options: RuntimeOptions)
{

  val directory = new Directory(initGroup)

  def defaultHandler(pkt: DatagramPacket): Unit

  protected var chans: Array[Channel] = null
  def channels: Array[Channel] = chans

  val dispatcher = new InstanceDispatcher(options)

  def close: Unit

  def start: Unit

}
