package psync

import io.netty.buffer.ByteBuf
import psync.formula._
import psync.utils.serialization.{KryoRegistration, KryoSerializer, KryoByteBufInput, KryoByteBufOutput}
import com.esotericsoftware.kryo.Kryo
import scala.reflect.ClassTag
import psync.runtime.Progress

/** A Round is the logical unit of time and communication in PSync.
 *
 * The rounds are parameterized by a type `A` which is the payload of the
 * messages sent during the round. To specify a round, the user needs to
 * extend this class and implement the `send` and `update` methods.
 * 
 * The round class provide some helper methods such as `broadcast`,
 * `exitAtEndOfRound`, and `expectedNbrMessages`.
 */
abstract class Round[A: ClassTag: KryoRegistration] extends EventRound[A] {

  //////////////////////////
  // user-defined methods //
  //////////////////////////
  
  // /* The initial progress conditions: either GoAhead or Timeout */
  def init: Progress = Progress.timeout(10) //FIXME a default value so we don't have to change everthing

  //  /** The message sent by the process during this round.*/
  //  def send(): Map[ProcessID,A]

  /** Update the local state according to the messages received.*/
  def update(mailbox: Map[ProcessID,A]): Unit

  /** How many messages are expected to be received by the process in this round.
    * This is not required but can be used by some runtime optimizations. */
  def expectedNbrMessages: Int = group.size

  ////////////////////
  // helper methods //
  ////////////////////

  /** Terminates the PSync instance at the end of the round. */
  protected final def exitAtEndOfRound(): Unit = {
    _continue = false
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
  
  protected var mailbox: Map[ProcessID, A] = Map.empty

  final def receive(sender: ProcessID, payload: A): Progress = {
    mailbox += (sender -> payload)
    if (mailbox.size >= expectedNbrMessages) Progress.goAhead
    else Progress.unchanged
  }

  override final def finishRound: Boolean = {
    update(mailbox)
    mailbox = Map.empty
    getContinue
  }

}

/** 1st try at "deconstructing" rounds, i.e., keeping the communication-closure without the batching of messages.
 *
 * The rounds are parameterized by a type `A` which is the payload of the
 * messages sent during the round.
 *
 * This version gives more control over how things evolves. The receive method
 * processes messages one-by-one and returns `true` if the runtime can move ahead.
 * 
 * The round class provide some helper methods such as `broadcast` and `sizeHint`.
 */
abstract class EventRound[A: ClassTag: KryoRegistration] extends RtRound {

  // /* The initial progress conditions */
  // def init: Progress

  /** The message sent by the process during this round.*/
  def send(): Map[ProcessID,A]

  /** Processes a message and returns wether there have been enough messages to proceed. */
  def receive(sender: ProcessID, payload: A): Progress
  
  /** Finishes the round and returns whether to continue (or terminate). */
  def finishRound(): Boolean = true

  /** An upper bound to the number of byte required for the payload.
    * a negative value is ignored and the global/default configuration option is used instead. */
  protected var sizeHint = -1

  /** Broadcast is a shortcut to send the same message to every participant. */
  protected final def broadcast[A](msg: A): Map[ProcessID,A] = {
    group.replicas.foldLeft(Map.empty[ProcessID,A])( (acc, r) => acc + (r.id -> msg) )
  }
  
  final protected[psync] def registerSerializer(kryo: Kryo) = {
    implicitly[KryoRegistration[A]].register(kryo)
  }
  
  final protected[psync] def packSend(kryo: Kryo, alloc: Int => KryoByteBufOutput, sending: ProcessID => Unit) = {
    val msgs = send()
    var progress = Progress.unchanged
    msgs.foreach{ case (dst, value) =>
      if (dst == group.self) {
        //can we skip the (de)serialization when sending to self
        progress = receive(dst, value)
      } else {
        val kryoOut = alloc(sizeHint)
        kryo.writeObject(kryoOut, value)
        sending(dst)
      }
    }
    progress
  }
  
  final protected[psync] def receiveMsg(kryo: Kryo, sender: ProcessID, kryoIn: KryoByteBufInput) = {
    val a = kryo.readObject(kryoIn, implicitly[ClassTag[A]].runtimeClass).asInstanceOf[A]
    receive(sender, a)
  }
  
  
}


/** RtRound is the interface of rounds used by the runtime.
 *  This interface does not expose the payload type to the runtime, i.e., not type parameter.
 */
abstract class RtRound {

  protected[psync] def registerSerializer(kryo: Kryo): Unit

  /** The initial progress conditions
   * @returns what is the default progress policy of this round
   */
  def init: Progress

  /** Send the messages
   * @param alloc the bytebuffer allocator to use
   * @param sending the callback taking care of sending the packets it assumes the data is contained in the buffer given in alloc
   * @returns whether we need to wait on messages or directly finish the round
   */
  protected[psync] def packSend(kryo: Kryo, alloc: Int => KryoByteBufOutput, sending: ProcessID => Unit): Progress

  /** A message has been reveived. This method is responsible for releasing the Butebuf.
   * @returns indicates if we can terminate the round early (no need to wait for more messages)
   */
  protected[psync] def receiveMsg(kryo: Kryo, sender: ProcessID, payload: KryoByteBufInput): Progress

  /** terminate the round (call the update method with the accumulated messages)
   * @returns indicates whether to terminate this instance
   */
  protected[psync] def finishRound: Boolean

  protected var group: psync.runtime.Group = null
  protected[psync] def setGroup(g: psync.runtime.Group) {
    group = g
  }
  

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
