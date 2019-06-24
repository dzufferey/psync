package example.byzantine.pbft

import psync._
import psync.utils.serialization._
import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}

object MessagesSerializer{
  implicit val reg1 = new KryoRegistration[Request] {
    override def registerClasses = Seq(classOf[Request], classOf[ViewChange])
  }
  implicit val reg2 = new KryoRegistration[ViewChange] {
    override def registerClasses = Seq(classOf[ViewChange])
    override def register(kryo: Kryo): Kryo = {
      val k1 = regSet[Request].register(kryo)
      val k2 = regList[(Long,Int)].register(k1)
      super.register(k2)
    }
  }
  implicit val reg3 = new KryoRegistration[ViewChangeAck] {
    override def registerClasses = Seq(classOf[ViewChangeAck])
    override def register(kryo: Kryo): Kryo = {
      val k1 = regMap[ProcessID,Int].register(kryo)
      super.register(k1)
    }
  }
  implicit val reg4 = new KryoRegistration[NewView] {
    override def registerClasses = Seq(classOf[NewView])
    override def register(kryo: Kryo): Kryo = {
      val k1 = regSet[Request].register(kryo)
      super.register(k1)
    }
  }
}
