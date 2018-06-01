package psync.utils

package object serialization {

  implicit lazy val regInt = new KryoRegistration[Int] { }
  
  implicit lazy val regLong = new KryoRegistration[Long] { }

  implicit lazy val regShort = new KryoRegistration[Short] { }

  implicit lazy val regByte = new KryoRegistration[Byte] { }

  implicit lazy val regIntPair = new KryoRegistration[(Int,Int)] {
    override def registerClasses = Seq(classOf[Tuple2[_,_]])
  }

  implicit lazy val regString = new KryoRegistration[String] {
    override def registerClasses = Seq(classOf[String])
  }

  implicit lazy val regByteArray= new KryoRegistration[Array[Byte]] {
    override def registerClasses = Seq(classOf[Array[Byte]])
  }

  //TODO automatic conversion from Serializer[A] tp KryoRegistration[A]

}
