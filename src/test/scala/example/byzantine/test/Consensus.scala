package example.byzantine.test

import psync._
import psync.runtime._
import psync.macros.Macros._
import psync.utils._
import psync.utils.serialization._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import scala.reflect.ClassTag
import java.security.MessageDigest
import example.ConsensusIO
import example.batching._

///a consensus algo in the style of PBFT single decisions

//Here are the messages
// number omitted, currently the instance, TODO should be included in the digest
case class PrePrepare(request: Array[Byte], digest: Array[Byte], view: Int) { }
case class Prepare(digest: Array[Byte], view: Int) { }
case class Commit(digest: Array[Byte], view: Int) { }

import MessagesSerializer._

// manual sync
class Bcp(useSync: Boolean, timeout: Long, shortTimeout: Long) extends Process[ConsensusIO[Array[Byte]]] {

  //variables
  var x = Array[Byte]()
  var prepared = false
  var callback: ConsensusIO[Array[Byte]] = null
  val md = MessageDigest.getInstance("SHA-256");
  var digest: Array[Byte] = null


  def init(io: ConsensusIO[Array[Byte]]) = i{
    prepared = false
    callback = io
    x = io.initialValue
    md.reset
    if (x != null) {
      digest = md.digest(x)
    }
  }

  def coord(phi: Int): ProcessID = new ProcessID((phi % n).toShort)

  def wrapRound[A: ClassTag: KryoRegistration](rnd: EventRound[A]): RtRound = {
    if (useSync) {
      new PessimisticByzantineSynchronizer(rnd, shortTimeout, timeout)
    } else {
      rnd
    }
  }


  val rounds = psync.macros.Macros.phase(
    //pre-prepare
    new EventRound[PrePrepare]{

      def init = Progress.strictTimeout(timeout)

      def send(): Map[ProcessID,PrePrepare] = {
        if (id == coord(r/3)) {
          broadcast(PrePrepare(x, digest, 0))
        } else {
          Map.empty[ProcessID,PrePrepare]
        }
      }

      def receive(sender: ProcessID, payload: PrePrepare) = {
        if (sender == coord(r/3)) {
          if (id != coord(r/3)) {
            md.reset
            x = payload.request
            digest = md.digest(x)
            if (!MessageDigest.isEqual(digest, payload.digest)) { //check the digest
              Logger("Bcp", Notice, s"$id, failed to check digest")
              x = null
              digest = null
            }
          }
          Progress.goAhead
        } else {
          Progress.unchanged
        }
      }

      override def finishRound(didTimeout: Boolean) = {
        if(x == null || didTimeout) { // abort on failing to get a request
          Logger("Bcp", Notice, s"$id, failed PrePrepare")
          callback.decide(null)
          false
        } else {
          true
        }
      }

    },

    //prepare
    new EventRound[Prepare]{

      var confirmed = 0

      def init = {
        confirmed = 0
        Progress.timeout(timeout)
      }

      def send(): Map[ProcessID,Prepare] = {
        broadcast(Prepare(digest, 0))
      }

      def receive(sender: ProcessID, payload: Prepare) = {
        if (MessageDigest.isEqual(payload.digest, digest)) {
          confirmed += 1
          if (confirmed > 2*n/3) {
            prepared = true
            Progress.goAhead
          } else Progress.unchanged
        } else {
          Progress.unchanged
        }
      }

    },

    //commit
    new EventRound[Commit]{

      var confirmed = 0

      def init = {
        confirmed = 0
        Progress.timeout(timeout)
      }

      def send(): Map[ProcessID,Commit] = {
        if (prepared) broadcast(Commit(digest, 0))
        else Map.empty[ProcessID,Commit]
      }

      def receive(sender: ProcessID, payload: Commit) = {
        if (MessageDigest.isEqual(payload.digest, digest)) {
          confirmed += 1
          if (confirmed > 2*n/3) Progress.goAhead
          else Progress.unchanged
        } else {
          Progress.unchanged
        }
      }

      override def finishRound(didTimeout: Boolean) = {
        if (!didTimeout) {
          callback.decide(x)
        } else {
          Logger("Bcp", Notice, s"id, failed Commit")
          callback.decide(null)
        }
        false //in all case terminate
      }

    }
  )

}

class ConsensusAlgo(rt: Runtime, useSync: Boolean, timeout: Long, shortTimeout: Long) extends Algorithm[ConsensusIO[Array[Byte]],Bcp](rt) {

  val spec = TrivialSpec

  def process = new Bcp(useSync, timeout, shortTimeout)

  def dummyIO = new ConsensusIO[Array[Byte]]{
    val initialValue = Array[Byte]()
    def decide(value: Array[Byte]): Unit = { }
  }
}
