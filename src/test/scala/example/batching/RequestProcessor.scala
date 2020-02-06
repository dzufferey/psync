package example.batching

import example.DecisionLog
import psync._
import psync.runtime._
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.TimeUnit
import io.netty.buffer.PooledByteBufAllocator

trait RequestProcessor {
  self: BatchingClient =>

  import BatchingClient.ForwardedBatch
  import Bytes._
  import self._

  // batches that needs to be proposed
  val pendingBatch = new ConcurrentLinkedQueue[Array[Byte]]
  // requests that still needs to be batched
  val pendingRequests = new ArrayBlockingQueue[(Int,Int,Int)](options.pending * options.batchSize)

  /** The client should use this method to submit new request */
  def propose(c: Int, k: Int, v: Int): Unit = {
    pendingRequests.put((c,k,v))
  }


  protected def submitBatch(batch: Array[Byte]): Unit = {
    if (isLeader) {
      lck.lock
      try {
        acquire // rate limiting
        val inst = tracker.nextInstance
        startInstance(inst, batch, Set.empty)
      } finally {
        lck.unlock
      }
    } else if (options.forward) {
      // forward the batch to the leader (process with id 0)
      val payload = PooledByteBufAllocator.DEFAULT.buffer()
      payload.writeLong(8)
      payload.writeInt(batch.size)
      payload.writeBytes(batch)
      rt.sendMessage(new ProcessID(0), Tag(0,0,ForwardedBatch,0), payload)
    }
  }

  // the thread that batches the requests and submit the batches
  val requestsProcessor = new Thread(new Runnable(){

    //an array to accumulate requests into batches
    val rs = options.batchSize * 12
    var request = Array.ofDim[Byte](rs)
    var idx = 0

    def run = {
      try{
        while(!Thread.interrupted) {
          //check batches forwarded by other replicas
          val b = pendingBatch.poll
          if (b != null) {
            submitBatch(b)
          }
          //check pending requests
          val req = pendingRequests.poll(options.rpTO, TimeUnit.MILLISECONDS)
          if (req != null) {
            val (c,k,v) = req
            assert(idx < rs + 12)
            intToBytes(c, request, idx)
            intToBytes(k, request, idx + 4)
            intToBytes(v, request, idx + 8)
            idx += 12
            if (idx >= rs) {
              submitBatch(request)
              request = Array.ofDim[Byte](rs)
              idx = 0
            }
          } else {
          }
        }
      } catch {
        case _: java.lang.InterruptedException => ()
      }
      Logger("BatchingClient", Info, "requestsProcessor, |pendingRequests| = " + pendingRequests.size + ", |pendingBatch| = " + pendingBatch.size)
    }
  })

}
