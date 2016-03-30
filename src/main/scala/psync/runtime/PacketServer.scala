package psync.runtime

import psync._
import io.netty.channel.Channel
import io.netty.channel.socket.DatagramPacket

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
