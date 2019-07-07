package example.byzantine.test

import psync._
import psync.utils.serialization._
import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}

object MessagesSerializer{
  implicit val reg1 = new KryoRegistration[PrePrepare] {
    override def registerClasses = Seq(classOf[PrePrepare])
    override def register(kryo: Kryo): Kryo = {
      val k1 = regByteArray.register(kryo)
      super.register(k1)
    }
  }
  implicit val reg2 = new KryoRegistration[Prepare] {
    override def registerClasses = Seq(classOf[Prepare])
    override def register(kryo: Kryo): Kryo = {
      val k1 = regByteArray.register(kryo)
      super.register(k1)
    }
  }
  implicit val reg4 = new KryoRegistration[Commit] {
    override def registerClasses = Seq(classOf[Commit])
    override def register(kryo: Kryo): Kryo = {
      val k1 = regByteArray.register(kryo)
      super.register(k1)
    }
  }
}
