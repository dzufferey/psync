package psync.utils.serialization

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}
import com.twitter.chill.EmptyScalaKryoInstantiator
import scala.reflect.ClassTag

object KryoSerializer {

  private val inst = new EmptyScalaKryoInstantiator

  def serializer = {
    val kryo = inst.newKryo
    kryo.setRegistrationRequired(true)
    kryo
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
