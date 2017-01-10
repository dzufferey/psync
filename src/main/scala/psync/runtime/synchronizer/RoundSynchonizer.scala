package psync.runtime.synchronizer

import psync._
import psync.runtime._
import psync.runtime.server.PacketServer
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import io.netty.channel.socket._
import io.netty.buffer.ByteBuf
import java.util.concurrent.TimeUnit

/* Turns a stream of individual messages into rounds.
 * The exact implementation of this layer depends on the fault model.
 */
abstract class RoundSynchonizer[IO,P <: Process[IO]](
  proc: P,
  pktServ: PacketServer, // to send the messages
  rt: psync.runtime.Runtime[IO,P],
  defaultHandler: DatagramPacket => Unit,
  options: RuntimeOptions)
{

  ///////////////
  // Interface //
  ///////////////

  /** Initialize the object.
   * if a message is received before the starting delay, start may be called early and followed by message.
   * @returns v > 0 if the messages for the first round should be delayed by v. Otherwise, 0.
   */
  def prepare(io: IO, g: Group, inst: Short): TO = {
    // init the process
    proc.setGroup(g)
    proc.init(io)

    // init this
    instance = inst
    grp = g
    n = g.size
    currentRound = 0

    // checkResources
    if (from == null || from.size != n) {
      from = Array.ofDim[Boolean](n)
      for (i <- 0 until n) from(i) = false
    }

    val delay = options.delayFirstSend.toLong
    if (delay > 0l) new TO(delay)
    else new TO(0l)
  }

  /** Start: send messages for the first round.
   * @returns the new timeout
   */
  def start: TO
  
  /** Handle packets received for this instance 
   * @returns the new timeout
   */
  def message(dp: DatagramPacket): TO

  /** Handle timeout
   * @returns the new timeout
   */
  def timeout: TO

  /** This instance should stop.
   *  Since there might be multiple threads working. It might take
   *  some time after this call returns until the instance actually
   *  finishes.
   * @returns true if the instance has stopped and should be recycled. false if it is the wrong instance or if it will take care of itself in the near future.
   */
  def interrupt(inst: Short): Boolean

  /////////////////
  // Local state //
  /////////////////
  
  protected var instance: Short = 0
  protected var grp: Group = null

  protected var n = 0
  protected var currentRound = 0

  //messages for the current round
  protected var from: Array[Boolean] = null
  
  //timeout options: they are persisted across instance execution
  protected var currentTimeout = options.timeout
  protected var didTimeOut = 0
  protected val adaptative = options.adaptative

  protected val earlyMoving = options.earlyMoving
  protected val sendWhenCatchingUp = options.sendWhenCatchingUp

  ////////////////////
  // Common methods //
  ////////////////////

  /** Forward the packet to the defaultHandler */
  protected def default(pkt: DatagramPacket) {
    rt.submitTask(new Runnable { def run = defaultHandler(pkt) })
  }
  
  protected def adaptTimeout {
    if (adaptative) {
      //TODO something amortized to avoid oscillations
      if (didTimeOut > 10) {
        didTimeOut = 0
        currentTimeout += 10
      } else if (didTimeOut < -20) {
        didTimeOut = 0
        currentTimeout -= 1
      }
    }
  }
  
  // responsible for freeing the msg if returns false
  protected def checkInstanceAndTag(msg: DatagramPacket): Boolean = {
    val tag = Message.getTag(msg.content)
    if (instance != tag.instanceNbr) { // wrong instance
      msg.release
      false
    } else if (tag.flag == Flags.normal) {
      // nothing to do we are fine
      true
    } else if (tag.flag == Flags.dummy) {
      Logger("RoundSynchonizer", Debug, grp.self.id + ", " + instance + "dummy flag (ignoring)")
      msg.release
      false
    } else {
      if (tag.flag == Flags.error) {
        Logger("RoundSynchonizer", Warning, "error flag (pushing to user)")
      }
      default(msg)
      false
    }
  }
  
  /** store a packet for the current round and perform duplicate check
   * @returns true if there is enough messages to go to the next round
   */
  protected def storePacket(sender: ProcessID, buf: ByteBuf) = {
    val id = sender.id
    if (!from(id)) {
      from(id) = true
      assert(Message.getTag(buf).roundNbr == currentRound, Message.getTag(buf).roundNbr + " vs " + currentRound)
      proc.receive(sender, buf)
    } else {
      // duplicate packet
      buf.release
      false
    }
  }

  protected def update = {
    Logger("RoundSynchonizer", Debug, grp.self.id + ", " + instance + " delivering for round " + currentRound)
    // clean
    for (i <- 0 until n) {
      from(i) = false
    }
    // update
    proc.update
  }

  protected def send {
    val tag = Tag(instance, currentRound)
    var sent = 0
    def sending(pid: ProcessID, payload: ByteBuf) {
      payload.setLong(0, tag.underlying)
      if (pid == grp.self) {
        storePacket(pid, payload)
      } else {
        pktServ.send(pid, payload)
      }
      sent += 1
    }
    proc.send(sending)
    Logger("RoundSynchonizer", Debug,
      grp.self.id + ", " + instance + " sending for round " + currentRound + " -> " + sent + "\n")
  }

  /** update followed by send */
  protected def toNextRound = {
    if (proc.update) {
      currentRound += 1
      send
      true
    } else {
      false
    }
  }

  protected def toNextAndAdapt = {
    if (toNextRound) {
      adaptTimeout
      new TO(currentTimeout)
    } else {
      new TO(-1l)
    }
  }

}
