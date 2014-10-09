package round.utils

import io.netty.buffer.ByteBuf
import io.netty.buffer.PooledByteBufAllocator

  //private val maxSize = 4096// 65536-1

object ByteBufAllocator extends PooledByteBufAllocator(true) {
//object ByteBufAllocator extends PooledByteBufAllocator(false) {

}
