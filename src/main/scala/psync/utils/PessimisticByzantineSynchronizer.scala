package psync.utils

import psync._
import psync.utils.serialization._
import scala.reflect.ClassTag

// takes a round and potentially adds message so that it is guaranteed to converge even with Byzantine processes
// works with f < n/3
// this does not make the round Byzantine tolerant, the round does still needs to deal with faulty messages. It only deal with synchronization.

//TODO test

class PessimisticByzantineSynchronizer[A: ClassTag: KryoRegistration](
    round: EventRound[A],       //the round to synchronize
    defaultTO: Long,            //a default timeout value
    waitMsg: Boolean = true    //if we can assume reliable channels between correct processes we can always wait for enogh messages and stay synchronized
  ) extends EventRound[Option[A]] {

  //progress status of the argument round
  protected var rProgress = Progress.unchanged

  //counting messages for the byzantine sychronization
  protected var nMsg = 0
  protected var nf = 0

  def init: Progress = {
    rProgress = round.init
    nMsg = 0
    nf = group.size - group.nbrByzantine
    val syncProgress =
      if (waitMsg) Progress.waitMessage
      else Progress.timeout(defaultTO)
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
      if (nMsg > nf || !waitMsg) Progress.timeout( defaultTO )
      else Progress.waitMessage
    Progress.lub(rProgress, syncProgress)
  }

  override def finishRound(didTimeout: Boolean): Boolean = {
    round.finishRound(didTimeout)
  }

}


