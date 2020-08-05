package example.batching

import psync.ProcessID
import psync.runtime.{Instance,Tag}
import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._
import scala.util.Random
import io.netty.buffer.{PooledByteBufAllocator, ByteBuf}
import BatchingClient.{AskDecision, Late, Decision}

// TODO consolidate the parts related to recovery here
// the main parts are
// 1. asking for a mising decision
// 2. processing a missing decison
// 3. processing a snapshot (if we are really late then will receive a snapshot instead of a decision)

trait Recovery {
  self: BatchingClient =>
  
  import Bytes._
  import self._

  def askDecision: Unit = {
    //pick someone to ask
    var askingTo = 0 //Random.nextInt(rt.group.size - 1)
    if (askingTo >= id) {
      askingTo = askingTo + 1
    }
    val payload = PooledByteBufAllocator.DEFAULT.buffer()
    payload.writeLong(8)
    rt.sendMessage(new ProcessID(askingTo.toShort), Tag(nextBatch,0,AskDecision,0), payload)
    Logger("BatchingClient", Notice, s"$id asking to $askingTo for decision $nextBatch")
  }
  
  def sendRecoveryInfo(inst: Short, dest: ProcessID) = {
    getDec(inst) match {
      case Some(d) =>
        val payload = PooledByteBufAllocator.DEFAULT.buffer()
        payload.writeLong(8)
        if (d != null && d.nonEmpty) {
          payload.writeInt(d.size)
          payload.writeBytes(d)
        } else {
          payload.writeInt(emp.size)
          payload.writeBytes(emp)
        }
        rt.sendMessage(dest, Tag(inst,0,Decision,0), payload)
        Logger("BatchingClient", Debug, s"$id sending decision to ${dest.id} for $inst")
      case None =>
        val tag = Tag((nextBatch-1).toShort,0,Late,0)
        val payload = if (options.packetSize > 0) PooledByteBufAllocator.DEFAULT.buffer(options.packetSize)
                      else PooledByteBufAllocator.DEFAULT.buffer()
        payload.writeLong(8)
        writeSnapshot(payload)
        rt.sendMessage(dest, tag, payload)
        Logger("BatchingClient", Debug, s"$id sending snapshot to ${dest.id}")
    }
  }


  //the size of the system in Byte (for snapshots)
  def snapshotSize = 6 * options.n 
  def canSerializeSnapshot = snapshotSize < options.packetSize 

  def writeSnapshot(buffer: ByteBuf) = {
    storeLock.lock
    try {
      buffer.writeShort((nextBatch - 1).toShort)
      var i = 0
      while (i < values.size) {
        buffer.writeInt(values(i))
        i += 1
      }
      i = 0
      while (i < values.size) {
        buffer.writeShort(versions(i))
        i += 1
      }
    } finally {
      storeLock.unlock
    }
  }

  def tryReadSnapshot(inst: Short, buffer: ByteBuf) = {
    storeLock.lock
    try {
      if (Instance.leq(nextBatch, inst)) {
        nextBatch = (inst + 1).toShort
        var i = 0
        while (i < values.size) {
          values(i) = buffer.readInt()
          i += 1
        }
        i = 0
        while (i < values.size) {
          versions(i) = buffer.readShort()
          i += 1
        }
        true
      } else {
        false
      }
    } finally {
      storeLock.unlock
      buffer.release
    }
  }


}
