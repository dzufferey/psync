package example.batching

import psync.ProcessID
import psync.runtime.{Instance,Tag}
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import scala.collection.mutable.PriorityQueue
import scala.util.Random
import io.netty.buffer.{PooledByteBufAllocator, ByteBuf}

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
  var nextBatch: Short = 1
  val values   = Array.fill[Int](options.n)(0)
  val versions = Array.fill[Short](options.n)(-1)


  // batches that have been accepted but still needs to be executed
  val acceptedBatch = new LinkedBlockingQueue[Either[(Short, Array[Byte]),(Short, ByteBuf)]]()

  def cleanup(inst: Short) = {
    val running = isRunning(inst)
    stopAndUpdateStarted(inst)
    if (running) {
      if (isLeader) {
        release
      } else if (options.eagerStart) {
        val inst = nextInstance
        startInstance(inst, emp, Set.empty)
      }
    }
  }

  def proposeDecision(inst: Short, data: Array[Byte]) = {
    val l = getLock(inst)
    l.lock
    try {
      //put the decision in the rolling log
      if (pushDecision(inst, data)) {
        Logger("BatchingClient", Debug, s"$id, proposeDecision for $inst ✓")
        acceptedBatch.add(Left(inst -> data))
        //clean-up instance
        lck.lock
        try {
          cleanup(inst)
        } finally {
          lck.unlock
        }
      } else {
        Logger("BatchingClient", Debug, s"$id proposeDecision for $inst ✗")
      }
    } finally {
      l.unlock
    }
  }

  def proposeSnapshot(inst: Short, buffer: ByteBuf) = {
    acceptedBatch.add(Right(inst -> buffer))
  }

  @inline final protected def processRequest(inst: Short, b: Array[Byte], base: Int): Unit = {
    val c = bytesToInt(b, base) 
    val k = bytesToInt(b, base + 4) 
    val v = bytesToInt(b, base + 8)
    //check that the request has not been superseded
    if (Instance.lt(versions(k), inst)) {
      values(k) = v
      versions(k) = inst
      if (log != null) {
        log.write(s"$inst\t$k\t$v")
        log.newLine()
      }
    }
    //TODO notify client
  }

  protected def processBatch(inst: Short, a: Array[Byte]): Unit = {
    storeLock.lock
    try {
      if (a != null && a.nonEmpty) { //null/empty means the proposer crashed before setting a value
        assert(a.size % 12 == 0)
        var b = 0
        while (b < a.size) {
          processRequest(inst, a, b)
          b += 12
          nbr += 1
        }
      }
      nextBatch = (inst + 1).toShort
    } finally {
      storeLock.unlock
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

    private def lateThreshold = reorderingQueue.size > options.rate * options.late

    protected def processSnapshot(inst: Short, buffer: ByteBuf) = {
      var old = nextBatch
      val didSmth = tryReadSnapshot(inst, buffer)
      // discard superceded decison from reorderingQueue
      while (!reorderingQueue.isEmpty && Instance.lt(reorderingQueue.head._1, nextBatch)) {
        reorderingQueue.dequeue
      }
      // inform the InstanceTracking (from old nextBatch to current nextBatch-1)
      if (didSmth) {
        lck.lock
        try {
          while (Instance.lt(old, nextBatch)) {
            cleanup(old)
            old = (old + 1).toShort
          }
        } finally {
          lck.unlock
        }
      }
    }

    def run: Unit = {
      try{
        while(!Thread.interrupted) {
          val req = acceptedBatch.poll(options.dpTO, TimeUnit.MILLISECONDS)
          if (req != null) {
            req match {
              case Left(req @ (inst, batch)) =>
                if (inst == nextBatch) {
                  processBatch(inst, batch)
                  // check in there are pending batches to process
                  while (!reorderingQueue.isEmpty && reorderingQueue.head._1 == nextBatch) {
                    val (inst, batch) = reorderingQueue.dequeue
                    processBatch(inst, batch)
                  }
                  if (!lateThreshold) {
                    if (isLate.get) {
                      Logger("BatchingClient", Info, s"$id, not late anymore")
                    }
                    isLate.set(false)
                  }
                } else if (Instance.lt(inst, nextBatch)) {
                  // batch superceded by snapshot, nothing to do
                } else {
                  // not the next batch, put in the reordering queue
                  if (!Instance.lt(nextBatch, inst)) {
                    Logger("DecisionProcessor", Critical, "Too far behind, commiting sucide (nextBatch = " + nextBatch + ", inst = " + inst + ")")
                    sys.exit(-1)
                  }
                  reorderingQueue += req
                  if (lateThreshold) {
                    if (!isLate.get) {
                      Logger("BatchingClient", Info, s"$id, late")
                    }
                    isLate.set(true)
                    if (!isRunning(nextBatch)) {
                      askDecision
                    }
                  }
                }
             case Right((inst, buffer)) =>
               processSnapshot(inst, buffer)
            }
          } else {
            // timeout, if late ask again for decisions
            if (isLate.get) {
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
