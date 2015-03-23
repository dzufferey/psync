package round.macros

import round.formula._
import round.logic._
import dzufferey.utils.Namer

class ByteBufInput(buffer: io.netty.buffer.ByteBuf) extends scala.pickling.binary.BinaryInput {
  import io.netty.buffer.ByteBuf
  assert(buffer.order == java.nio.ByteOrder.BIG_ENDIAN)
  def getByte() = buffer.readByte
  def getChar() = buffer.readInt.toChar
  def getShort() = buffer.readShort
  def getInt() = buffer.readInt
  def getLong() = buffer.readLong
  def getFloat() = buffer.readFloat
  def getDouble() = buffer.readDouble
  def getBytes(target: Array[Byte], len: Int): Unit = {
    buffer.readBytes(target, 0, len)
  }
}

class ByteBufOutput(buffer: io.netty.buffer.ByteBuf) extends scala.pickling.binary.BinaryOutput {
  import io.netty.buffer.ByteBuf
  assert(buffer.order == java.nio.ByteOrder.BIG_ENDIAN)
  def result: Array[Byte] = null
  def ensureCapacity(capacity: Int) {
    if (capacity > 0) {
      buffer.ensureWritable(capacity)
      if (buffer.writableBytes < capacity) {
        throw new java.nio.BufferOverflowException()
      }
    }
  }
  def putByte(value: Byte) = buffer.writeByte(value)
  def putChar(value: Char) = buffer.writeInt(value.toInt)
  def putShort(value: Short) = buffer.writeShort(value)
  def putInt(value: Int) = buffer.writeInt(value)
  def putLong(value: Long) = buffer.writeLong(value)
  def putFloat(value: Float) = buffer.writeFloat(value)
  def putDouble(value: Double) = buffer.writeDouble(value)
  def putBytes(value: Array[Byte], len: Int) = buffer.writeBytes(value, 0, len)
}

trait Serialization {
  self: Impl =>
  import c.universe._

  //$tpt -> protected def serialize(payload: $tpt, out: _root_.io.netty.buffer.ByteBuf, withLength: Boolean = true, offset: Int = 8): Int
  //$tpt -> protected def deserialize(in: _root_.io.netty.buffer.ByteBuf, withLength: Boolean = true, offset: Int = 8): $tpt
  
  
  def picklingIO(tpt: Tree) = List(
      q"""protected def serialize(payload: $tpt, out: _root_.io.netty.buffer.ByteBuf) {
        import scala.pickling._
        import scala.pickling.Defaults._
        import binary._
        out.writerIndex(out.writerIndex() + _root_.round.runtime.Tag.size)
        payload.pickleTo(new _root_.round.macros.ByteBufOutput(out))
      }""",
      q"""protected def deserialize(in: _root_.io.netty.buffer.ByteBuf): $tpt = {
        import scala.pickling._
        import scala.pickling.Defaults._
        import binary._
        in.readerIndex(in.readerIndex() + _root_.round.runtime.Tag.size)
        val pickle = BinaryPickle(new _root_.round.macros.ByteBufInput(in))
        pickle.unpickle[$tpt]
      }"""
    )

  def primitiveIO(tpt: Tree, write: List[Tree], read: List[Tree]) = {
    val errMessage = "error while deserializing " + tpt
    List(
      q"""protected def serialize(payload: $tpt, out: _root_.io.netty.buffer.ByteBuf) = {
        out.writerIndex(out.writerIndex() + _root_.round.runtime.Tag.size)
        ..$write
      }""",
      q"""protected def deserialize(in: _root_.io.netty.buffer.ByteBuf): $tpt = {
        try {
          in.readerIndex(in.readerIndex() + _root_.round.runtime.Tag.size)
          ..$read
        } catch {
          case e: Throwable =>
            Console.err.println($errMessage)
            throw e
        }
      }"""
    )
  }

  def primitiveType(t: Type) = {
    import definitions._
    t =:= BooleanTpe ||
    t =:= ByteTpe ||
    t =:= CharTpe ||
    t =:= ShortTpe ||
    t =:= IntTpe ||
    t =:= LongTpe ||
    t =:= FloatTpe ||
    t =:= DoubleTpe
  }

  def primitiveRead(t: Type): Tree = {
    import definitions._
    if (t =:= BooleanTpe) {
      q"in.readBoolean()"
    } else if (t =:= ByteTpe) {
      q"in.readByte()"
    } else if (t =:= CharTpe) {
      q"in.readChar()"
    } else if (t =:= ShortTpe) {
      q"in.readShort()"
    } else if (t =:= IntTpe) {
      q"in.readInt()"
    } else if (t =:= LongTpe) {
      q"in.readLong()"
    } else if (t =:= FloatTpe) {
      q"in.readFloat()"
    } else if (t =:= DoubleTpe) {
      q"in.readDouble()"
    } else {
      sys.error("not primitive")
    }
  }
 
  def primitiveWrite(t: Type, value: Tree): Tree = {
    import definitions._
    if (t =:= BooleanTpe) {
      q"out.writeBoolean($value)"
    } else if (t =:= ByteTpe) {
      q"out.writeByte($value)"
    } else if (t =:= CharTpe) {
      q"out.writeChar($value)"
    } else if (t =:= ShortTpe) {
      q"out.writeShort($value)"
    } else if (t =:= IntTpe) {
      q"out.writeInt($value)"
    } else if (t =:= LongTpe) {
      q"out.writeLong($value)"
    } else if (t =:= FloatTpe) {
      q"out.writeFloat($value)"
    } else if (t =:= DoubleTpe) {
      q"out.writeDouble($value)"
    } else {
      sys.error("not primitive")
    }
  }

  def tupleWrite(args: List[Type], value: Tree): List[Tree] = {
    val name = Array(TermName("_1"), TermName("_2"), TermName("_3"), TermName("_4"))
    val res = args.zipWithIndex.map{ case (t, idx) => val m = name(idx); primitiveWrite(t, q"$value.$m") }
    //println("tupleWrite: " + res.mkString(", "))
    res
  }

  def tupleRead(args: List[Type]): List[Tree] = {
    val name = Array(q"_1", q"_2", q"_3", q"_4")
    val (reads, vars) = args.map( t => {
      val v = TermName(c.freshName("tmp"))
      val r = primitiveRead(t) 
      (ValDef(Modifiers(), v, TypeTree(t), r), Ident(v))
    }).unzip
    val res = reads ::: List(q"(..$vars)")
    //println("tupleRead: " + res.mkString(", "))
    res
  }

  def serializationMethods(tpt: Tree): List[Tree] = {
    val t = tpt.tpe
    if (primitiveType(t)) {
      val wr = List(primitiveWrite(t, q"payload"))
      val rd = List(primitiveRead(t))
      primitiveIO(tpt, wr, rd)
    } else {
      t match {
        case IsTuple(args) if args.forall(primitiveType) =>
          val wr = tupleWrite(args, q"payload")
          val rd = tupleRead(args)
          primitiveIO(tpt, wr, rd)
        case _ =>
          //TODO string
          //TODO options
          println("using pickling on " + showRaw(tpt))
          picklingIO(tpt)
      }
    }
  }

}
