package example.batching

import psync.ProcessID
import psync.runtime.{Instance,Tag}
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import scala.collection.mutable.PriorityQueue
import scala.util.Random
import io.netty.buffer.PooledByteBufAllocator

trait DecisionProcessor {
  self: BatchingClient =>

  import BatchingClient.AskDecision
  import Bytes._
  import self._

  // to log the decisions

  val log: java.io.BufferedWriter = options.logFile match {
    case Some(l) => new java.io.BufferedWriter(new java.io.FileWriter(l + "_" + id + ".log"))
    case None => null
  }

  if (log != null) {
    log.write("inst\tkey\tvalue")
    log.newLine()
  }

  // state of the system

  val values   = Array.fill[Int](options.n)(0)
  val versions = Array.fill[Short](options.n)(-1)
  // batches that have been accepted but still needs to be executed
  val acceptedBatch = new LinkedBlockingQueue[(Short, Array[Byte])]()


  def proposeDecision(inst: Short, data: Array[Byte]) = {
    val l = getLock(inst)
    l.lock
    try {
      //put the decision in the rolling log
      if (pushDecision(inst, data)) {
        Logger("BatchingClient", Debug, id + ", proposeDecision for " + inst + " ✓")
        acceptedBatch.add(inst -> data)
        //
        lck.lock
        try {
          tracker.stopAndUpdateStarted(inst)
          if (isLeader) {
            release
          } else if (options.eagerStart) {
            val inst = tracker.nextInstance
            startInstance(inst, emp, Set.empty)
          }
        } finally {
          lck.unlock
        }
      } else {
        Logger("BatchingClient", Debug, id + ", proposeDecision for " + inst + " ✗")
      }
    } finally {
      l.unlock
    }
  }


  @inline final protected def processRequest(inst: Short, b: Array[Byte], base: Int) {
    val c = bytesToInt(b, base) 
    val k = bytesToInt(b, base + 4) 
    val v = bytesToInt(b, base + 8)
    //check that the request has not been superseded
    if (Instance.lt(versions(k), inst)) {
      values(k) = v
      versions(k) = inst
      if (log != null) {
        log.write(inst + "\t" + k + "\t" + v)
        log.newLine()
      }
    }
    //TODO notify client
  }

  protected def processBatch(inst: Short, a: Array[Byte]) {
    if (a != null && a.nonEmpty) { //null/empty means the proposer crashed before setting a value
      assert(a.size % 12 == 0)
      var b = 0
      while (b < a.size) {
        processRequest(inst, a, b)
        b += 12
        nbr += 1
      }
    }
  }


  // the thread that executes the commands and notify the clients
  val decisionProcessor = new Thread(new Runnable(){
    object BatchOrdering extends Ordering[(Short, Array[Byte])] {
      // assumes that the instance number are unique
      def compare(a: (Short, Array[Byte]), b: (Short, Array[Byte])) = {
        Instance.compare(b._1, a._1) // swap a,b for lowest batch first
      }
    }
    private val reorderingQueue = new PriorityQueue[(Short, Array[Byte])]()(BatchOrdering)
    private var nextBatch: Short = 1
    private def lateThreshold = reorderingQueue.size > options.rate * options.late
    private def askDecision {
      //pick someone to ask
      var askingTo = scala.util.Random.nextInt(rt.group.size)
      while (askingTo == id) {
        askingTo = scala.util.Random.nextInt(rt.group.size)
      }
      val payload = PooledByteBufAllocator.DEFAULT.buffer()
      payload.writeLong(8)
      rt.sendMessage(new ProcessID(askingTo.toShort), Tag(nextBatch,0,AskDecision,0), payload)
      Logger("BatchingClient", Info, id + " asking to " + askingTo + " for decision " + nextBatch)
    }
    def run = {
      try{
        while(!Thread.interrupted) {
          val req = acceptedBatch.poll(options.dpTO, TimeUnit.MILLISECONDS)
          if (req != null) {
            val (inst, batch) = req
            if (inst == nextBatch) {
              processBatch(inst, batch)
              nextBatch = (nextBatch + 1).toShort
              // check in there are pending batches to process
              while (!reorderingQueue.isEmpty && reorderingQueue.head._1 == nextBatch) {
                val (inst, batch) = reorderingQueue.dequeue
                processBatch(inst, batch)
                nextBatch = (nextBatch + 1).toShort
              }
              if (!lateThreshold) {
                if (isLate.get) {
                    Logger("BatchingClient", Info, id + ", not late anymore")
                }
                isLate.set(false)
              }
            } else {
              // not the next batch, put in the reordering queue
              if (!Instance.lt(nextBatch, inst)) {
                Logger("DecisionProcessor", Critical, "Too far behind, commiting sucide (nextBatch = " + nextBatch + ", inst = " + inst + ")")
                sys.exit(-1)
              }
              reorderingQueue += req
              if (lateThreshold) {
                if (!isLate.get) {
                    Logger("BatchingClient", Info, id + ", late")
                }
                isLate.set(true)
                if (!tracker.isRunning(nextBatch)) {
                  askDecision
                }
              }
            }
          } else {
            if (isLate.get) {
              // late and timeout, ask again
              askDecision
            }
          }
        }
      } catch {
        case _: java.lang.InterruptedException => ()
      }
      Logger("BatchingClient", Info, "decisionProcessor, nextBatch = " + nextBatch + ", |reorderingQueue| = " + reorderingQueue.size)
    }
  })

}
