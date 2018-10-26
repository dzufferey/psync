package psync.utils.serialization

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}
import com.twitter.chill.EmptyScalaKryoInstantiator
import scala.reflect.ClassTag
import psync.{ProcessID, Time}
import scala.collection.generic.CanBuildFrom

object KryoSerializer {

  private val inst = new EmptyScalaKryoInstantiator

  def serializer = {
    val kryo = inst.newKryo
    kryo.setRegistrationRequired(true)
    kryo
  }

}


class ProcessIDSerializer extends Serializer[ProcessID] {
  setImmutable(true)
  def write(kryo: Kryo, output: Output, t: ProcessID) = {
    output.writeShort(t.id)
  }
  def read(kryo: Kryo, input: Input, ct: Class[ProcessID]): ProcessID = {
    val s = input.readShort()
    new ProcessID(s)
  }
}


class TimeSerializer extends Serializer[Time] {
  setImmutable(true)
  def write(kryo: Kryo, output: Output, t: Time) = {
    output.writeInt(t.toInt)
  }
  def read(kryo: Kryo, input: Input, ct: Class[Time]): Time = {
    val i = input.readInt()
    new Time(i)
  }
}


class GenericOptionSerializer extends Serializer[Option[_]] {
  setImmutable(true)
  def write(kryo: Kryo, output: Output, opt: Option[_]) = opt match {
    case None =>
      output.writeByte(-1)
    case Some(v) =>
      output.writeByte(0)
      kryo.writeClassAndObject(output, v)
  }
  def read(kryo: Kryo, input: Input, ct: Class[Option[_]]): Option[_] = {
    val b = input.readByte()
    if (b != 0) {
      None
    } else {
      val v = kryo.readClassAndObject(input)
      Some(v)
    }
  }
}


class OptionSerializer[A: ClassTag] extends Serializer[Option[A]] {
  setImmutable(true)
  protected val cls: Class[A] = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]
  def write(kryo: Kryo, output: Output, opt: Option[A]) = opt match {
    case None =>
      output.writeByte(-1)
    case Some(v) =>
      output.writeByte(0)
      kryo.writeObject(output, v)
  }
  def read(kryo: Kryo, input: Input, ct: Class[Option[A]]): Option[A] = {
    val b = input.readByte()
    if (b != 0) {
      None
    } else {
      val v = kryo.readObject(input, cls).asInstanceOf[A]
      Some(v)
    }
  }
}

class CollectionSerializer[A, B <: Traversable[A]](implicit ct: ClassTag[A], bf: CanBuildFrom[Nothing, A, B]) extends Serializer[B] {
  setImmutable(true)
  protected val cls: Class[A] = ct.runtimeClass.asInstanceOf[Class[A]]
  def write(kryo: Kryo, output: Output, coll: B) = {
    val s = coll.size
    output.writeInt(s)
    coll.foreach( kryo.writeObject(output, _) )
  }
  def read(kryo: Kryo, input: Input, ct: Class[B]): B = {
    val builder = bf()
    var i = input.readInt()
    while (i > 0) {
      i = i - 1
      val v = kryo.readObject(input, cls).asInstanceOf[A]
      builder += v
    }
    builder.result
  }
}
