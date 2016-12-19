package psync.runtime.handler

import psync._
import psync.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import io.netty.channel.socket._
import io.netty.buffer.ByteBuf
import java.util.concurrent.TimeUnit

/* Turns a stream of individual messages into rounds.
 * The exact implementation of this layer depends on the fault model.
 */
class BenignSynchonizer[IO,P <: Process[IO]](
  proc: P,
  pktServ: PacketServer, // to send the messages
  rt: psync.runtime.Runtime[IO,P],
  defaultHandler: DatagramPacket => Unit,
  options: RuntimeOptions) extends RoundSynchonizer(proc, pktServ, rt, defaultHandler, options)
{

  protected var running = false

  /** Start: send messages for the first round.
   * @returns the new timeout
   */
  def start: TO = {
    running = true
    send
    new TO(currentTimeout)
  }

  /** Handle packets received for this instance 
   * @returns the new timeout
   */
  def message(dp: DatagramPacket): TO = {
    // discard wrong tag, wrong instance
    if (!running) {
      new TO(-1l)
    } else if (!checkInstanceAndTag(dp)) {
      new TO(0l)
    } else {
      val msgRound = Message.getTag(dp.content).roundNbr
      var late = msgRound - currentRound
      if (late < 0) {
        // late message, ignore
        dp.release
        new TO(0l)
      } else {
        var more = true
        // catching up
        while (more && late > 0) {
          if (sendWhenCatchingUp) {
            more = toNextRound
          } else {
            more = proc.update
            currentRound += 1
          }
          late -= 1
        }
        if (!more) {
          new TO(-1l)
        } else {
          val sender = grp.inetToId(dp.sender)
          val hasEnoughMessages = storePacket(sender, dp.content)
          if (hasEnoughMessages && earlyMoving) {
            // enough message to go ahead
            toNextAndAdapt
          } else {
            // still waiting for more messages
            new TO(0l)
          }
        }
      }
    }
  }

  /** Handle timeout
   * @returns the new timeout
   */
  def timeout: TO = {
    if (running) {
      toNextAndAdapt
    } else {
      new TO(-1l)
    }
  }

  /** This instance should stop.
   *  Since there might be multiple threads working. It might take
   *  some time after this call returns until the instance actually
   *  finishes.
   * @returns true if the instance has stopped and should be recycled. false if it will take care of itself in the near future.
   */
  def interrupt(inst: Short): Boolean = {
    if (running && instance == inst) {
      running = false
      proc.cleanUp
      true
    } else {
      false
    }
  }

}
