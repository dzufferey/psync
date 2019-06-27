package psync.utils

import psync._
import psync.utils.serialization._
import scala.reflect.ClassTag

// takes a round and potentially adds message so that it is guaranteed to converge even with Byzantine processes
// works with f < n/3
// this does not make the round Byzantine tolerant, the round does still needs to deal with faulty messages. It only deal with synchronization.

//TODO may need two timeouts
// - long timeouts until receive n-f
// - short timeouts after n-f
// - go ahead when has n messages

class PessimisticByzantineSynchronizer[A: ClassTag: KryoRegistration](
    round: EventRound[A],       //the round to synchronize
    shortTimeout: Long,         //a short timeout value (to get some more messages afte we have #msg to progress)
    longTimeout: Long           //a long timeout value (to wait for the minimum #msg to progress)
  ) extends EventRound[Option[A]] {

  //progress status of the argument round
  protected var rProgress = Progress.unchanged

  //counting messages for the byzantine sychronization
  protected var nMsg = 0
  protected var nf = 0
  protected var n = 0

  def init: Progress = {
    rProgress = round.init
    nMsg = 0
    n = group.size
    nf = n - group.nbrByzantine
    val syncProgress =
      if (longTimeout > 0) Progress.timeout(longTimeout)
      else Progress.waitMessage
    Progress.lub(rProgress, syncProgress)
  }

  def send(): Map[ProcessID,Option[A]] = {
    val rSend = round.send
    val acc = Map.empty[ProcessID,Option[A]]
    group.replicas.foldLeft(acc)( (acc, r) => acc + (r.id -> rSend.get(r.id)) )
  }

  def receive(sender: ProcessID, payload: Option[A]): Progress = {
    nMsg += 1
    for (msg <- payload) {
      rProgress = round.receive(sender, msg).orElse(rProgress) //if 'unchanged' keep the old one
    }
    val syncProgress =
      if (nMsg > nf) { // enough messages to progress
        if (nMsg == n || shortTimeout <= 0) Progress.goAhead
        else Progress.timeout( shortTimeout )
      } else { // need more messages
        if (longTimeout > 0) Progress.timeout( longTimeout )
        else Progress.waitMessage
      }
    Progress.lub(rProgress, syncProgress)
  }

  override def finishRound(didTimeout: Boolean): Boolean = {
    round.finishRound(didTimeout)
  }

}


