package example

import round._
import round.runtime._
import round.macros.Macros._
import round.utils.{Timer, ByteBufAllocator}

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import java.net.InetSocketAddress
import io.netty.util.{TimerTask, Timeout}

import java.util.concurrent.{Semaphore, ConcurrentLinkedQueue}
    
import scala.pickling._
import binary._

sealed abstract class MembershipOp
case class AddReplica(address: String, port: Int) extends MembershipOp
case class RemoveReplica(id: ProcessID) extends MembershipOp

abstract class MembershipIO {
  val initialValue: Option[MembershipOp]
  def decide(value: Option[MembershipOp]): Unit
}

//TODO we need a basic consensus algorithms for the MembershipOps
class BasicConsensus extends Algorithm[MembershipIO] {

  import VarHelper._
  import SpecHelper._

  val spec = ???
  
  def process(id: ProcessID, io: MembershipIO) = ???
  //p(new Process(id) {
  //  val rounds = Array[Round]( ??? )
  //})

}

object DynamicMembership extends dzufferey.arg.Options {

  final val Heartbeat = 3
  final val Recover = 4
  final val View = 5
  
  ///////////////
  // Variables //
  ///////////////
  
  //keep the current verion number (for the algorithm)
  private var instanceNbr: Short = 0

  //the current view nbr (needed if we want to combine it with another algorithm that take care of doing actual work)
  private var viewNbr = 0
  private var view: Directory = null

  //when is the last time we heard of some guy (ProcessID → last heart beat)
  private val lastHearOf = Array.ofDim[Long](64)
  
  /////////////
  // Options //
  /////////////

  val usage = "..."
  
  var address = "127.0.0.1"
  newOption("-a", dzufferey.arg.String( str => address = str), "replica address (default is 127.0.0.1)")
  var port = 8889
  newOption("-p", dzufferey.arg.Int( i => port = i), "port (default is 8889)")
  
  var masterPort: Option[Int] = None
  newOption("-rp", dzufferey.arg.Int( i => masterPort = Some(i)), "master port")
  var masterAddress: Option[String] = None
  newOption("-ra", dzufferey.arg.String( str => masterAddress = Some(str)), "master address")
  
  newOption("-v", dzufferey.arg.Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", dzufferey.arg.Unit(() => Logger.lessVerbose), "decrease the verbosity level.")

  //////////////////////////////////
  // Dispatcher and custom logics //
  //////////////////////////////////

  private val pending = new ConcurrentLinkedQueue[MembershipOp]()
  private val semaphore = new Semaphore(1) //at most one consensus at the time
  
  private def startNextConsensus(nbr: Option[Short], op: Option[MembershipOp], msg: Set[Message] = Set.empty) = {
    if (semaphore.tryAcquire) {
      val nbrOk = nbr.isEmpty || nbr.get == instanceNbr + 1 //start because of sequence #
      val opOk = op.isDefined || !pending.isEmpty //start because of op
      if (nbrOk || opOk) {
        instanceNbr = (instanceNbr + 1).asInstanceOf[Short]
        val init =
          if (op.isEmpty) {
            val p = pending.poll
            if (p == null) None
            else Some(p)
          } else op
        val io = new MembershipIO {
          val initialValue = init
          def decide(value: Option[MembershipOp]) { onDecision(value) }
        }
        rt.startInstance(nbr.get, io, msg)
      } else {
        //already started
        semaphore.release()
        msg.foreach(_.release)
      }
    } else {
      if (op.isDefined) pending.add(op.get)
      msg.foreach(_.release)
    }
  }
  
  def onDecision(dec: Option[MembershipOp]) {
    try {
      dec match {
        case Some(AddReplica(address, port)) =>
          Logger("DynamicMembership", Notice, "adding replica " + address + ":" + port)
          //the new replica gets a new ID
          val newId = view.firstAvailID //this is a deterministic operation
          view.addReplica(Replica(newId, address, port))
          viewNbr += 1
          sendRecoveryInfo(newId)
     
        case Some(RemoveReplica(id)) =>
          val self = view.self
          if (id == self) {
            Logger.logAndThrow("DynamicMembership", Error, "We were kicked out of the view!")
          }
          Logger("DynamicMembership", Notice, "removing replica " + id)
          view.removeReplica(id)
          view.compact //this is a deterministic operation
          viewNbr += 1
     
        case None =>
          Logger("DynamicMembership", Warning, "consensus did not converge to a decision")
      }
    } finally {
      //let other threads go
      semaphore.release
    }
    //check if there are pending operations
    startNextConsensus(None, None)
  }


  def defaultHandler(msg: Message) {
    val flag = msg.tag.flag
    if (flag == Flags.normal || flag == Flags.dummy) {
      //check version number to know whether we are in synch
      val inst = msg.tag.instanceNbr
      val expected = (instanceNbr + 1).asInstanceOf[Short]

      if (inst == expected) {
        startNextConsensus(Some(expected), None, Set(msg))

      } else if (inst > expected) { //TODO inst/expected can loop around
        startRecovery(msg.senderId)
        msg.release

      } else {
        //late or race to start instance -> discard the message
        //or should we send a View message to the guy ? 
        msg.release
      }

    } else if (flag == Flags.error) {
      Logger("DynamicMembership", Warning, "received an error message")
    } else if (flag == Heartbeat) {
      onHeartBeat(msg)
      msg.release
    } else if (flag == Recover) {
      onRecoverMessage(msg)
      msg.release
    } else if (flag == View) {
      onViewMessage(msg)
      msg.release
    } else {
      Logger("DynamicMembership", Warning, "received an unkown message: flag = " + flag)
      msg.release
    }
  }

  def onRecoverMessage(msg: Message) {
    Logger("DynamicMembership", Notice, "recover message from " + msg.senderId)
    val (host, port) = msg.getContent[(java.lang.String,scala.Int)]
    val address = new InetSocketAddress(host, port)
    view.getSafe(address) match {
      case Some(replica) => sendRecoveryInfo(replica.id)
      case None =>
        //if not part of the group, propose to add him
        startNextConsensus(None, Some(AddReplica(host, port)))
    }
  }

  def sendRecoveryInfo(dest: ProcessID) {
    Logger("DynamicMembership", Notice, "sending recovery info to " + dest)
    val tag = Tag(0,0,View,0)
    val payload = ByteBufAllocator.buffer(2048)
    payload.writeLong(8)
    val content = (viewNbr, dest.id, view.asList)
    val array = content.pickle.value
    payload.writeBytes(array)
    rt.sendMessage(dest, tag, payload)
  }

  def onViewMessage(msg: Message) {
    Logger("DynamicMembership", Notice, "view message")
    val (v,id,replicas) = msg.getContent[(scala.Int,scala.Short,List[Replica])]
    val group = Group(new ProcessID(id), replicas)
    //TODO sync ?
    viewNbr = v
    view.group = group
  }

  def startRecovery(dest: ProcessID) {
    val tag = Tag(0,0,Recover,0)
    val payload = ByteBufAllocator.buffer(256)
    payload.writeLong(8)
    val array = (address -> port).pickle.value
    payload.writeBytes(array)
    rt.sendMessage(dest, tag, payload)
  }

  ////////////////
  // heart beat //
  ////////////////

  def onHeartBeat(msg: Message) {
    Logger("DynamicMembership", Debug, "heart beat")
    val src = msg.senderId
    if (view contains src) {
      assert(src.id < lastHearOf.size)
      lastHearOf(src.id) = java.lang.System.currentTimeMillis() //TODO do we need sync ?
    }
  }
  
  private val crashTO = 1000
  private val heartbeatPeriod = 100
  private val heartbeatTask = new TimerTask {
    def run(to: Timeout) {

      //send msg to others
      val self = view.self
      if (self.id >= 0) {
        val others = view.others
        for (o <- others) {
          val tag = Tag(0,0,Heartbeat,0)
          val payload = ByteBufAllocator.buffer(16)
          payload.writeLong(0l) //leav space for the tag
          payload.writeShort(self.id) //not really needed
          rt.sendMessage(o.id, tag, payload)
        }
       
        //check whether someone timed out
        val cur = java.lang.System.currentTimeMillis()
        val late = for (o <- others if o.id.id < lastHearOf.size && lastHearOf(o.id.id) + crashTO < cur) yield o.id
        if (late.size > others.size / 2) {
          //we are problably disconnected from the network, wait until is gets better
        } else {
          for (id <- late) pending.add(RemoveReplica(id))
          startNextConsensus(None, None)
        }
      }

      //set the new timeout
      heartbeatTO = Timer.newTimeout(this, heartbeatPeriod)
    }
  }
  protected var heartbeatTO: Timeout = Timer.newTimeout(heartbeatTask, heartbeatPeriod)


  ///////////
  // setup //
  ///////////

  private val rt = new round.runtime.RunTime[MembershipIO](new BasicConsensus)

  def setup() {
    val isMaster = masterPort.isDefined || masterAddress.isDefined
    assert(!isMaster || (masterPort.isDefined && masterAddress.isDefined))
    val id = if (isMaster) new ProcessID(0) else new ProcessID(-1)
    val self = Replica(id, address, port)
    val peers =
      if (isMaster) {
        List(Replica(new ProcessID(0), masterAddress.get, masterPort.get))
      } else {
        //we are the master
        List(self)
      }
    rt.startService(defaultHandler(_), peers, Map("id" -> id.id.toString, "port" -> port.toString))
    view = rt.directory
    if (!isMaster) {
      startRecovery(new ProcessID(0))
    }
  }

  
  def main(args: Array[java.lang.String]) {
    //parse the args
    apply(args)

    setup()

    //clean-up on ctrl-c
    java.lang.Runtime.getRuntime().addShutdownHook(
      new Thread() {
        override def run() {
          heartbeatTO.cancel
          if (rt != null)
            rt.shutdown
        }
      }
    )
  }

}
