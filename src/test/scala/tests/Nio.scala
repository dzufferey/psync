package tests

import java.net.DatagramPacket
import java.nio.channels.DatagramChannel
import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicLong
import java.nio.ByteBuffer

class NioServer(port: Int) {
  
  val nbr = new AtomicLong(0l)

  val channel = DatagramChannel.open()
  channel.bind(new InetSocketAddress("127.0.0.1", port))
  
  def start {
    try {
      val buffer = ByteBuffer.allocateDirect(2048)
      while(true) {
        val addr = channel.receive(buffer)
        buffer.clear
        nbr.incrementAndGet
      }
    } catch {
      case e: Exception => ()
    }
  }

  def shutdown = {
    channel.close
    nbr.get
  }

}

class NioClient(clientPort: Int, serverPort: Int) {
  
  val channel = DatagramChannel.open()
  channel.bind(new InetSocketAddress("127.0.0.1", clientPort))
  
  def start {
  }

  def loop {
    val addr = new InetSocketAddress("127.0.0.1", serverPort)
    val buffer = ByteBuffer.allocateDirect(24)
    buffer.putLong(0)
    buffer.putLong(0)
    buffer.putLong(0)
    while(true) {
      channel.send(buffer, addr)
    }
  }

  def close {
    channel.close
  }

}

object Nio {
  
  var srv = true
  var s: NioServer = null
  var c: NioClient = null
  var begin = 0l
  
  def main(args: Array[java.lang.String]) {
    if (args.length == 1)
      srv = true
    else if (args.length == 2)
      srv = false
    else
      sys.error("wrong # of args")
    
    if (srv) {
      s = new NioServer(args(0).toInt)
      begin = java.lang.System.currentTimeMillis()
      s.start
    } else {
      c = new NioClient(args(0).toInt, args(1).toInt)
      c.start
      c.loop
    }

  }

  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        if (srv) {
          val versionNbr = s.shutdown
          val end = java.lang.System.currentTimeMillis()
          val duration = (end - begin) / 1000
          println("#pkt = " + versionNbr + ", Δt = " + duration + ", throughput = " + (versionNbr/duration))
        } else {
          c.close
        }
      }
    }
  )
}
