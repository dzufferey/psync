package round.runtime

import round._
import Algorithm._
import io.netty.buffer.ByteBuf
import round.utils.LogLevel._
import round.utils.Logger

class ProcessWrapper(p:Process) {
  def setGroup(g: Group) = p.setGroup(g)
  def send(): Seq[(ByteBuf,ProcessID)] = p.send.toSeq
  def update(msgs: Seq[(ByteBuf,ProcessID)]) = p.update(msgs.toSet)
}


class RunTime[IO](val alg: Algorithm[IO]) {

  private var srv: Option[PacketServer] = None

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
        val wrapper = new ProcessWrapper(process)
        val predicate = new PredicateLayer(grp, instanceId, s.channel, wrapper)
        s.registerInstance(predicate)
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
    val param = param1 ++ additionalOpt

    //create the group
    val me = param("id").toShort
    val grp = Group(me, peers)

    //start the server
    val port = grp.get(me).port
    Logger("RunTime", Info, "starting service on port " + port)
    val pktSrv = new PacketServer(port, grp, defaultHandler)
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
