package psync

import io.netty.buffer.ByteBuf
import psync.formula._
import psync.utils.serialization.{KryoRegistration, KryoSerializer, KryoByteBufInput, KryoByteBufOutput}
import scala.reflect.ClassTag

/** A Round is the logical unit of time and communication in PSync.
 *
 * The rounds are parameterized by a type `A` which is the payload of the
 * messages sent during the round. To specify a round, the user needs to
 * extend this class and implement the `send` and `update` methods.
 * 
 * The round class provide some helper methods such as `broadcast`,
 * `exitAtEndOfRound`, and `terminate`.
 */
abstract class Round[A: ClassTag: KryoRegistration] extends RtRound {

  //////////////////////////
  // user-defined methods //
  //////////////////////////

  /** The message sent by the process during this round.*/
  def send(): Map[ProcessID,A]

  /** Update the local state according to the messages received.*/
  def update(mailbox: Map[ProcessID,A]): Unit

  /** How many messages are expected to be received by the process in this round.
    * This is not required but can be used by some runtime optimizations. */
  def expectedNbrMessages: Int = group.size

  /** An upper bound to the number of byte requires for the payload (including tag.size).
    * a negative value is ignored and the global configuration option is used instead. */
  protected var sizeHint = -1

  ////////////////////
  // helper methods //
  ////////////////////

  /** Terminates the PSync instance at the end of the round. */
  protected final def exitAtEndOfRound(): Unit = {
    _continue = false
  }

  /** Broadcast is a shortcut to send the same message to every participant. */
  protected final def broadcast[A](msg: A): Map[ProcessID,A] = {
    group.replicas.foldLeft(Map.empty[ProcessID,A])( (acc, r) => acc + (r.id -> msg) )
  }

  /////////////////////
  // for the runtime //
  /////////////////////

  private var _continue = true
  protected[psync] def getContinue = {
    val c = _continue
    _continue = true
    c
  }
  
  private var group: psync.runtime.Group = null
  protected[psync] def setGroup(g: psync.runtime.Group) {
    group = g
  }
  
  protected var mailbox: Map[ProcessID, A] = Map.empty

  protected val serializer = implicitly[KryoRegistration[A]].register(KryoSerializer.serializer)
  
  final protected[psync] def packSend(alloc: Int => KryoByteBufOutput, sending: (ProcessID, KryoByteBufOutput) => Unit) = {
    val msgs = send()
    msgs.foreach{ case (dst, value) =>
      if (dst == group.self) {
        //can we skip the (de)serialization when sending to self
        mailbox += (dst -> value)
      } else {
        val kryoOut = alloc(sizeHint)
        serializer.writeObject(kryoOut, value)
        sending(dst, kryoOut)
      }
    }
    mailbox.size >= expectedNbrMessages
  }

  final protected[psync] def receiveMsg(sender: ProcessID, kryoIn: KryoByteBufInput): Boolean = {
    val a = serializer.readObject(kryoIn, implicitly[ClassTag[A]].runtimeClass).asInstanceOf[A]
    mailbox += (sender -> a)
    mailbox.size >= expectedNbrMessages
  }

  final protected[psync] def finishRound: Boolean = {
    update(mailbox)
    mailbox = Map.empty
    getContinue
  }
  
}


/** RtRound is the interface of rounds used by the runtime. */
abstract class RtRound {
  
  /** send the messages
   * @param alloc the bytebuffer allocator to use
   * @param sending the callback taking care of sendinf the packets
   * @returns whether we need to wait on messages or directly finish the round
   */
  protected[psync] def packSend(alloc: Int => KryoByteBufOutput, sending: (ProcessID, KryoByteBufOutput) => Unit): Boolean

  /** A message has been reveived. This method is responsible for releasing the Butebuf.
   * @returns indicates if we can terminate the round early (no need to wait for more messages)
   */
  protected[psync] def receiveMsg(sender: ProcessID, payload: KryoByteBufInput): Boolean

  /** terminate the round (call the update method with the accumulated messages)
   * @returns indicates whether to terminate this instance
   */
  protected[psync] def finishRound: Boolean

  protected[psync] def setGroup(g: psync.runtime.Group): Unit

}

class RoundSpec {

  //////////////////////
  // for verification //
  //////////////////////

  import verification._

  //macros will take care of overriding those methods
  def auxSpec: Map[String, AuxiliaryMethod] = Map.empty
  def rawTR: RoundTransitionRelation = new RoundTransitionRelation(
    True(), Variable("s"), True(), Variable("u"), Nil, Nil, Nil )
  def sendStr: String = ""
  def updtStr: String = ""

}
