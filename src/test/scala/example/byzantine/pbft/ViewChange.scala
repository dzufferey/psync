package example.byzantine.pbft

import psync._
import psync.runtime._
import psync.macros.Macros._
import psync.utils._
import psync.utils.serialization._

// A variation of PBFT view change without signature

// 〈ViewChange, v+1, h, 𝓒, 𝓟, 𝓠, i〉
// h is the latest stable checkpoint
// 𝓒 pairs 〈sequence number, digest〉of checkpoints
// 𝓟 requests prepared in previous views
// 𝓠 requests pre-prepared in prepared views
// request 〈n,d,v〉where n is the request number, v is the view, and d is the digest

// 〈ViewChangeAck, v+1, i, j, d〉
// i is the ID of the sender
// j is the ID of the replica triggering the view change
// d is the digest of the message being acknowledged

// compute view change at primary
// 𝓢 view-change messages accumulated at the new primary
// - select the max stable checkpoint:
//   〈h, d〉= max { 〈n, d〉| ∃ S, S′ ⊆ 𝓢. |S| > 2f ∧ |S′| > f ∧ ∀ m ∈ S. m.h ≤ n ∧ ∀ m ∈ S′. 〈n,d〉∈ m.C }
// - compute new view
//   for n ∈ [h,h+L] do
//     if ∃ m ∈ 𝓢. 〈n,d,v〉∈ m.𝓟 ∧ ( 
//        |{ m′ ∈ 𝓢 | m′.h < n ∧ ∀ 〈n,d′,v′〉∈ m′.𝓟. v′ < v  ∨  (v′ = v  ∧  d′ = d)}| > 2f ∨
//        |{ m′ ∈ 𝓢 | ∃ 〈n,d′,v′〉∈ m′.𝓠. v′ ≥ v  ∧ d′ = d}| > f )
//     then select〈n,d〉
//     else if |{ m ∈ 𝓢 | m.h < n ∧ 〈n,_,_〉∉ m.𝓟 }| > 2f
//     then null request for n
// 𝓧 = 〈h, d〉
// 𝓥 = selected entries
// send 〈NewView, v+1, 𝓥, 𝓧〉 to all

case class Request(number: Long, digest: Int, view: Int) { }

case class ViewChange(  newView: Int,
                        stableCheckPoint: Long,
                        checkpoints: List[(Long,Int)],
                        prepared: Set[Request],
                        prePrepared: Set[Request] )
case class ViewChangeAck( newView: Int,
                          digests: Map[ProcessID,Int] )
case class NewView( newView: Int,
                    stableCheckPoint: Long, 
                    toPrePrepare: Set[Request] )

import MessagesSerializer._

abstract class Log {
  def coord(view: Int): ProcessID
  def getCoord = coord(view)
  def view: Int
  def prepared: Set[Request]
  def prePrepared: Set[Request]
  def lastCheckpoint: Long //sequence number of last checkpoint
  def checkpoints: List[(Long,Int)] //sequence number and digest
  def update(vcs: Iterable[ViewChange]): Unit // compute a new views
  def tryUpdate(nv: NewView, vcs: Iterable[ViewChange]): Unit // get a new view, update if it can be confirmed
}


class ViewChangeProcess(timeout: Long) extends Process[Log] {
  
  var log: Log = null
  var distributedState: Map[ProcessID,ViewChange] = Map.empty
  var nextView: Int = -1

  def init(_log: Log): Unit = {
    log = _log
    nextView = log.view + 1
    distributedState = Map.empty
  }

  val rounds = phase(
    new EventRound[ViewChange]{
      var received = 0
      def init = {
        received = 0
        Progress.timeout( timeout )
      }
      def send: Map[ProcessID,ViewChange] = {
        broadcast(ViewChange(nextView, log.lastCheckpoint, log.checkpoints, log.prepared, log.prepared))
      }
      def receive(sender: ProcessID, v: ViewChange) = {
        if (v.newView == nextView) {
          distributedState += (sender -> v)
          received += 1
        }
        if (received > 2*n) Progress.goAhead
        else Progress.unchanged
      }
    },
    new EventRound[ViewChangeAck]{
      var received = 0
      var confirmations: Map[ProcessID,Int] = Map.empty
      def init = {
        received = 0
        var confirmations = Map.empty
        if (id != log.coord(nextView)) Progress.goAhead
        else Progress.timeout( timeout )
      }
      def send: Map[ProcessID,ViewChangeAck] = {
        Map(log.coord(nextView) -> ViewChangeAck(nextView, distributedState.map{ case (pid, vc) => (pid, vc.hashCode) }))
      }
      def receive(sender: ProcessID, v: ViewChangeAck) = {
        if (id == log.coord(nextView) && v.newView == nextView) {
          received += 1
          for ( (pid, ack) <- v.digests ) {
            if (distributedState.contains(pid) && 
                ack == distributedState(pid).hashCode) {
              confirmations += (pid -> (confirmations.getOrElse(pid, 0) + 1))
            }
          }
        }
        if (received > 2*n) Progress.goAhead
        else Progress.unchanged
      }
      override def finishRound(didTimeout: Boolean) = {
        if (id == log.coord(nextView)) {
          distributedState = distributedState.filter{ case (pid, _) => confirmations(pid) > n/3 } //keeps the logs confirmed by a least one correct process
          if (distributedState.size > 2*n/3) { //enough logs to compute the next view
            log.update(distributedState.values)
          }
        }
        true
      }
    },
    new EventRound[NewView]{
      def init = {
        Progress.timeout( timeout )
      }
      def send: Map[ProcessID,NewView] = {
        if (id == log.coord(nextView) && log.view == nextView) {
          broadcast(NewView(nextView, log.lastCheckpoint, log.prePrepared))
        } else {
          Map.empty
        }
      }
      def receive(sender: ProcessID, v: NewView) = {
        if (sender == log.coord(nextView)) {
          log.tryUpdate(v, distributedState.values) //use the messages from the 1st round to confirm the new state
          Progress.goAhead
        } else {
          Progress.unchanged
        }
      }
      override def finishRound(didTimeout: Boolean) = {
        if (log.view == nextView) { //success
          false //terminate
        } else {
          nextView += 1 //retry
          true //
        }
      }
    }
  )

}
