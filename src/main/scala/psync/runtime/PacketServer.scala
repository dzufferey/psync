package psync.runtime

import psync._
import io.netty.channel.Channel
import io.netty.channel.socket.DatagramPacket

abstract class PacketServer(
    executor: java.util.concurrent.Executor,
    port: Int,
    initGroup: Group,
    _defaultHandler: Message => Unit, //defaultHandler is responsible for releasing the ByteBuf payload
    options: RuntimeOptions)
{

  val directory = new Directory(initGroup)

  protected[psync] def defaultHandler(pkt: DatagramPacket) {
    val msg = new Message(pkt, directory.group)
    _defaultHandler(msg)
  }

  protected[psync] val dispatcher = new InstanceDispatcher(options)

  def close: Unit

  def start: Unit

  def send(pkt: DatagramPacket): Unit

}
