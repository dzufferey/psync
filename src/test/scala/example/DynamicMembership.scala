package example

import round._
import round.runtime._
import round.macros.Macros._
import round.utils.{Timer, ByteBufAllocator}

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

import java.net.InetSocketAddress
import io.netty.util.{TimerTask, Timeout}

import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{Semaphore, ConcurrentLinkedQueue}
    
import scala.pickling._
import binary._
          
import scala.math.Ordered._

sealed abstract class MembershipOp extends Ordered[MembershipOp] {
  def compare(that: MembershipOp) = {
    (this, that) match {
      case (AddReplica(a1,p1), AddReplica(a2,p2)) => (a1 -> p1) compare (a2 -> p2)
      case (AddReplica(a1,p1), RemoveReplica(i2)) => -1
      case (RemoveReplica(i1), AddReplica(a2,p2)) => 1
      case (RemoveReplica(i1), RemoveReplica(i2)) => i1.id compare i2.id
    }
  }
}
case class AddReplica(address: String, port: Int) extends MembershipOp
case class RemoveReplica(id: ProcessID) extends MembershipOp

abstract class MembershipIO {
  val initialValue: MembershipOp
  def decide(value: MembershipOp): Unit
}

//this is the OTR but for MembershipOp
class BasicConsensus extends Algorithm[MembershipIO] {

  import VarHelper._
  import SpecHelper._

  val x = new LocalVariable[MembershipOp](null)
  val decision = new LocalVariable[Option[MembershipOp]](None) //TODO as ghost
  val after = new LocalVariable[Int](1)
  
  //TODO
  val spec = new Spec {
    val safetyPredicate = f(true)
    val livnessPredicate = List( )
    val invariants = List( )
    val properties = List( )
  }
  
  def process(id: ProcessID, io: MembershipIO) = p(new Process(id) {
    
    x <~ io.initialValue

    val rounds = Array[Round](
      rnd(new Round{

        type A = MembershipOp
        
        //FIXME this needs to be push inside the round, otherwise it crashes the compiler (bug in macros?)
        def mmor(mailbox: Set[(MembershipOp, ProcessID)]): MembershipOp = {
          //TODO requires not empty
          val byValue = mailbox.groupBy(_._1)
          val m = byValue.minBy{ case (v, procs) => (-procs.size.toLong, v) }
          m._1
        } ensuring { v1 =>
          mailbox.map(_._1).forall( v2 =>
            mailbox.filter(_._1 == v1).size > mailbox.filter(_._1 == v2).size || v1.compare(v2) <= 0
          )
        }
        
        def send(): Set[(MembershipOp, ProcessID)] = {
          broadcast(x) //macro for (x, All)
        }

        def update(mailbox: Set[(MembershipOp, ProcessID)]) {
          if (mailbox.size > 2*n/3) {
            val v = mmor(mailbox)
            x <~ v
            if (mailbox.filter(msg => msg._1 == v).size > 2*n/3) {
              if (decision.isEmpty) {
                io.decide(v)
              }
              decision <~ Some(v);
            }
            //terminate after decision
            if (decision.isDefined) {
              after <~ after -1
              if (after < 0)
                terminate()
            }
          }

        }
      })
    )
  })

}

object DynamicMembership extends dzufferey.arg.Options with DecisionLog[MembershipOp] {

  final val Heartbeat = 3
  final val Recover = 4
  final val View = 5
  final val Decision = 6
  
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
  
  var masterAddress: Option[String] = None
  newOption("-ma", dzufferey.arg.String( str => masterAddress = Some(str)), "master address")
  var masterPort: Option[Int] = None
  newOption("-mp", dzufferey.arg.Int( i => masterPort = Some(i)), "master port")
  
  newOption("-v", dzufferey.arg.Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", dzufferey.arg.Unit(() => Logger.lessVerbose), "decrease the verbosity level.")

  //////////////////////////////////
  // Dispatcher and custom logics //
  //////////////////////////////////

  private val pending = new ConcurrentLinkedQueue[MembershipOp]()
  private val semaphore = new Semaphore(1) //at most one consensus at the time
  private val decisionLock = new ReentrantLock
  
  private def startNextConsensus(nbr: Option[Short], op: Option[MembershipOp], msg: Set[Message] = Set.empty) = {
    if (semaphore.tryAcquire) {
      val nbrOk = nbr.isEmpty || nbr.get == instanceNbr + 1 //start because of sequence #
      val opOk = op.isDefined || !pending.isEmpty //start because of op
      if (nbrOk && opOk) {
        instanceNbr = (instanceNbr + 1).asInstanceOf[Short]
        val init = op.getOrElse(pending.poll)
        assert(init != null, "cannot decide on initial value") 
        val io = new MembershipIO {
          val inst = instanceNbr
          val initialValue = init
          def decide(value: MembershipOp) { onDecision(inst, value) }
        }
        rt.startInstance(instanceNbr, io, msg)
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

  def onDecision(inst: Short, dec: MembershipOp) {
    val l = decisionLocks(decIdx(inst))
    l.lock
    try {
      if (inst == instanceNbr && getDec(inst).isEmpty) { //check for race with recovery
        pushDecision(inst, dec)
        dec match {
          case AddReplica(address, port) =>
            Logger("DynamicMembership", Notice, "adding replica " + address + ":" + port)
            //the new replica gets a new ID
            val newId = view.firstAvailID //this is a deterministic operation
            lastHearOf(newId.id) = java.lang.System.currentTimeMillis()
            view.addReplica(Replica(newId, address, port))
            viewNbr += 1
            sendRecoveryInfo(newId)
            Logger("DynamicMembership", Info, "current view (#"+viewNbr+"):" + view)
      
          case RemoveReplica(id) =>
            val self = view.self
            if (id == self) {
              Logger.logAndThrow("DynamicMembership", Error, "We were kicked out of the view!")
            }
            Logger("DynamicMembership", Notice, "removing replica " + id)
            view.removeReplica(id)
            view.compact //this is a deterministic operation
            initTO
            viewNbr += 1
            Logger("DynamicMembership", Info, "current view (#"+viewNbr+"):" + view)
      
        }
      }
    } finally {
      //let other threads go
      l.unlock
      semaphore.release
    }
    //check if there are pending operations
    startNextConsensus(None, None)
  }


  def defaultHandler(msg: Message) {
    val flag = msg.tag.flag
    Logger("DynamicMembership", Debug, "defaultHandler(" + msg.tag + ")")
    if (flag == Flags.normal || flag == Flags.dummy) {
      //check version number to know whether we are in synch
      val inst = msg.tag.instanceNbr
      val expected = (instanceNbr + 1).asInstanceOf[Short]

      if (inst == expected) {
        val bytes = msg.getPayLoad
        val i = BinaryPickle(bytes).unpickle[MembershipOp]
        //val i = msg.getContent[MembershipOp]
        startNextConsensus(Some(expected), Some(i), Set(msg))

      } else if (Instance.lt(expected, inst)) {
        startRecovery(msg.senderId)
        msg.release

      } else {
        //late or race to start instance -> discard the message
        trySendDecision(msg)
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
    } else if (flag == Decision) {
      onDecisionMessage(msg)
      msg.release
    } else {
      Logger("DynamicMembership", Warning, "received an unkown message: flag = " + flag)
      msg.release
    }
  }

  def onRecoverMessage(msg: Message) {
    Logger("DynamicMembership", Notice, "recover message from " + msg.senderId)
    val (host, port) = msg.getContent[(String,Int)]
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
    val content = (viewNbr, instanceNbr, dest.id, view.asList)
    val array = content.pickle.value
    payload.writeBytes(array)
    rt.sendMessage(dest, tag, payload)
  }

  def onViewMessage(msg: Message) {
    Logger("DynamicMembership", Notice, "view message")
    val (v,inst,id,replicas) = msg.getContent[(Int,Short,Short,List[Replica])]
    val group = Group(new ProcessID(id), replicas)
    if (v > viewNbr) {
      assert(inst > instanceNbr)
      instanceNbr = inst
      viewNbr = v
      view.group = group
      initTO
      Logger("DynamicMembership", Info, "current view (#"+viewNbr+"):" + view)
    }
  }

  def startRecovery(dest: ProcessID) {
    val tag = Tag(0,0,Recover,0)
    val payload = ByteBufAllocator.buffer(256)
    payload.writeLong(8)
    val array = (address -> port).pickle.value
    payload.writeBytes(array)
    rt.sendMessage(dest, tag, payload)
  }

  def onDecisionMessage(msg: Message) {
    val inst = msg.instance
    val dec = msg.getContent[MembershipOp]
    onDecision(inst, dec)
    rt.stopInstance(inst)
  }

  def trySendDecision(msg: Message) {
    getDec(msg.instance) match {
      case Some(d) =>
        val tag = Tag(msg.instance,0,Decision,0)
        val payload = ByteBufAllocator.buffer(256)
        payload.writeLong(8)
        val array = d.pickle.value
        payload.writeBytes(array)
        rt.sendMessage(msg.senderId, tag, payload)
      case None =>
        //otherwise the replica will have to recover by other means
    }
  }

  ////////////////
  // heart beat //
  ////////////////

  def initTO = {
    val t = java.lang.System.currentTimeMillis()
    for (p <- view.others) {
      lastHearOf(p.id.id) = t
    }
    if (heartbeatTO == null)
      heartbeatTO = Timer.newTimeout(heartbeatTask, heartbeatPeriod)
  }

  def onHeartBeat(msg: Message) {
    Logger("DynamicMembership", Debug, "heart beat")
    val src = msg.senderId
    if (view contains src) {
      assert(src.id < lastHearOf.size)
      lastHearOf(src.id) = java.lang.System.currentTimeMillis() //do we need sync ?
    }
  }
  
  private val crashTO = 5000
  private val heartbeatPeriod = 1000
  private val heartbeatTask = new TimerTask {
    def run(to: Timeout) {

      Logger("DynamicMembership", Debug, "running heartbeat task")

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
        if (!late.isEmpty) {
          Logger("DynamicMembership", Debug, "heartbeat did not hear from: " + late.mkString(", "))
          for (id <- late) {
            pending.add(RemoveReplica(id))
          }
          startNextConsensus(None, None)
        }
      }

      //set the new timeout
      heartbeatTO = Timer.newTimeout(this, heartbeatPeriod)
    }
  }
  protected var heartbeatTO: Timeout = null


  ///////////
  // setup //
  ///////////

  private val rt = new round.runtime.RunTime[MembershipIO](new BasicConsensus)

  def setup() {
    val isMaster = masterPort.isEmpty && masterAddress.isEmpty
    assert(isMaster || (masterPort.isDefined && masterAddress.isDefined))
    val id = if (isMaster) new ProcessID(0) else new ProcessID(-1)
    val self = Replica(id, address, port)
    val peers =
      if (isMaster) {
        List(self)
      } else {
        List(Replica(new ProcessID(0), masterAddress.get, masterPort.get))
      }
    rt.startService(defaultHandler(_), peers, Map("id" -> id.id.toString, "port" -> port.toString))
    view = rt.directory
    if (isMaster) {
      Logger("DynamicMembership", Info, "Starting as master")
      initTO
    } else {
      Logger("DynamicMembership", Info, "Connecting to master")
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
          if (heartbeatTO != null)
            heartbeatTO.cancel
          if (rt != null)
            rt.shutdown
        }
      }
    )
  }

}
