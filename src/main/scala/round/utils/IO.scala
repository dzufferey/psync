package round.utils

import java.io._

object IO {

  def storeInFile(file: File, data: Array[Byte]): Unit = {
    val fileOut = new DataOutputStream( new FileOutputStream(file))
    fileOut.write(data, 0, data.length)
    fileOut.close
  }

  def storeInFile(file: String, data: Array[Byte]): Unit = storeInFile(new File(file), data)

  def storeInTempFile(prefix: String, suffix: String, uploadDirectory: File, data: Array[Byte]) = {
    val storage = java.io.File.createTempFile(prefix, suffix, uploadDirectory)
    storeInFile(storage, data)
    storage
  }

  def writeInFile(file: File, data: String): Unit = {
    val fileOut = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
    fileOut.write(data)
    fileOut.flush
    fileOut.close
  }
  
  def writeInFile(file: String, data: String): Unit = writeInFile(new File(file), data)
  
  def writeInFile(file: String, data: BufferedWriter => Unit): Unit = {
    val fileOut = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
    data(fileOut)
    fileOut.flush
    fileOut.close
  }

  //TODO the append to file version
  //...

  def readTextFile(file: String): String = {
    val fileIn = new BufferedReader(new FileReader(file))
    val builder = new StringBuilder(1000)
    while(fileIn.ready) {
      builder ++= fileIn.readLine + "\n"
    }
    fileIn.close
    builder.toString
  }

  def readStdin: String = {
    val read = new scala.collection.mutable.StringBuilder
    var line = scala.Console.in.readLine
    while (line != null) {
      read ++= line
      read ++= "\n"
      line = scala.Console.in.readLine
    }
    read.toString
  }

}

/** prefix as a writer */
class PrefixingWriter(prefix: String, base: OutputStream) extends Writer {
    
  val out = new OutputStreamWriter(base)
  var needPrefix = true

  private def ifPrefixNeeded {
    if (needPrefix) {
      out.write(prefix)
      needPrefix = false
    }
  }

  def write(cbuf: Array[Char], off: Int, len: Int) {
    val max = math.min(cbuf.size - off, off + len)
    var start = off
    while (start < max) {
      val idx = cbuf.indexOf('\n', start)
      if (idx == -1 || idx >= max) {
        ifPrefixNeeded
        out.write(cbuf, start, len + off - start)
        start = max
      } else {
        ifPrefixNeeded
        out.write(cbuf, start, idx - start + 1)
        start = math.min(max, idx + 1)
        if (start < max) {
          needPrefix = true
        }
      }
    }
  }

  def close {
    out.close
  }

  def flush {
    out.flush
  }

}

