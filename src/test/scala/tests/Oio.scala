package tests

import java.net.DatagramPacket
import java.net.DatagramSocket
import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicLong

class OioServer(port: Int) {
  
  val nbr = new AtomicLong(0l)

  val socket = new DatagramSocket(port)
  
  def start {
    try {
      val pkt = new DatagramPacket(Array.ofDim[Byte](2048), 2048)
      while(true) {
        socket.receive(pkt)
        nbr.incrementAndGet
      }
    } catch {
      case e: Exception => ()
    }
  }

  def shutdown = {
    socket.close
    nbr.get
  }

}

class OioClient(clientPort: Int, serverPort: Int) {
  
  val socket = new DatagramSocket(clientPort)
  
  def start {
  }

  def loop {
    val addr = new InetSocketAddress("127.0.0.1", serverPort)
    val pkt = new DatagramPacket(Array.ofDim[Byte](24), 24, addr)
    while(true) {
      socket.send(pkt)
    }
  }
  
  def close {
    socket.close
  }


}

object Oio {
  
  var srv = true
  var s: OioServer = null
  var c: OioClient = null
  var begin = 0l
  
  def main(args: Array[java.lang.String]) {
    if (args.length == 1)
      srv = true
    else if (args.length == 2)
      srv = false
    else
      sys.error("wrong # of args")
    
    if (srv) {
      s = new OioServer(args(0).toInt)
      begin = java.lang.System.currentTimeMillis()
      s.start
    } else {
      c = new OioClient(args(0).toInt, args(1).toInt)
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
