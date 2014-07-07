package round.runtime

import round._
import Algorithm._
import round.predicate._
import io.netty.buffer.ByteBuf
import round.utils.LogLevel._
import round.utils.Logger


class RunTime[IO](val alg: Algorithm[IO]) {

  private var srv: Option[PacketServer] = None

  private var options = Map.empty[String, String]

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
        val process = alg.process(grp.self, io)
        process.setGroup(grp)
        val predicate = new ToPredicate(grp, instanceId, s.channel, s.dispatcher, process, options)
        //first round
        predicate.send
        //msg that are already received
        for(m <- messages) {
          val pkt = m.repack(grp, Tag(instanceId, 1))
          predicate.receive(pkt)
        }
      case None =>
        sys.error("service not running")
    }
  }

  /** Start the service that ... */
  def startService(
    defaultHandler: Message => Unit,
    configFile: String = "conf.xml",
    additionalOpt: Map[String, String] = Map.empty
  ) {
    if (srv.isDefined) {
      //already running
      return
    }

    //parse config
    val (peers,param1) = Config.parse(configFile)
    options = param1 ++ additionalOpt

    //create the group
    val me = options("id").toShort
    val grp = Group(me, peers)

    //start the server
    val port = grp.get(me).port
    Logger("RunTime", Info, "starting service on port " + port)
    val pktSrv = new PacketServer(port, grp, defaultHandler, options)
    srv = Some(pktSrv)
    pktSrv.start
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


}
