package example.batching

object Bytes {

  @inline final def bytesToInt(b: Array[Byte], base: Int) = {
     b(base + 3) & 0xFF |
    (b(base + 2) & 0xFF) << 8 |
    (b(base + 1) & 0xFF) << 16 |
    (b(base    ) & 0xFF) << 24;
  }

  @inline final def intToBytes(a: Int, b: Array[Byte], base: Int) = {
    b(base + 3) = (a & 0xFF).toByte
    b(base + 2) = ((a >>  8) & 0xFF).toByte
    b(base + 1) = ((a >> 16) & 0xFF).toByte
    b(base    ) = ((a >> 24) & 0xFF).toByte
  }

}
