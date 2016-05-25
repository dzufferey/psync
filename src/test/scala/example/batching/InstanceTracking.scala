package example.batching

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import psync.runtime.Instance


/** To keep track of the what is running
 *  This object is not thread safe! */
class InstanceTracking {

  /** the # of the highest started instance */
  var started: Short = 0
  /** what is currently running */
  var running = scala.collection.mutable.Set[Short]()
  /** with the sliding window, there may be gap (# < started but not yet started) */
  var pending = scala.collection.mutable.TreeSet[Short]()

  assertTrackingInvariant

  override def toString = {
    "started: " + started +
    "\nrunning: " + running.mkString(", ") +
    "\npending: " + pending.mkString(", ")
  }

  def canStart(inst: Short) = {
    Instance.lt(started, inst) || pending(inst)
  }

  protected def updateStarted(inst: Short) {
    var oldStarted = started
    started = Instance.max(started, inst)
    oldStarted = (oldStarted + 1).toShort
    while(Instance.lt(oldStarted, started)) {
      pending += oldStarted
      oldStarted = (oldStarted + 1).toShort
    }
  }

  def start(inst: Short) {
    pending -= inst
    running += inst
    updateStarted(inst)
    assertTrackingInvariant
  }

  def stop(inst: Short) {
    assert(running(inst), "not running " + inst + "\n" + toString)
    running -= inst
    assertTrackingInvariant
  }

  def stopAndUpdateStarted(inst: Short) {
    updateStarted(inst)
    running -= inst
    pending -= inst
    assertTrackingInvariant
  }

  def isRunning(inst: Short) = running contains inst

  def nextInstance = {
    if (pending.isEmpty) {
      (started + 1).toShort
    } else {
      val m1 = pending.min
      val m2 = pending.max
      Instance.min(m1, m2)
    }
  }

  def trackingInvariant = {
    running.forall( Instance.leq(_, started) ) &&
    pending.forall( Instance.lt(_, started) ) &&
    pending.forall( !running.contains( _ ) )
  }

  def assertTrackingInvariant {
    if (!trackingInvariant) {
      Logger.logAndThrow("InstanceTracking", Error, toString)
    }
  }

}
