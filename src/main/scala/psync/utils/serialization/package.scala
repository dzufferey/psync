package psync.utils

import psync.{ProcessID, Time}

package object serialization {
  
  implicit lazy val regBool = new KryoRegistration[Boolean] { }

  implicit lazy val regInt = new KryoRegistration[Int] { }
  
  implicit lazy val regLong = new KryoRegistration[Long] { }

  implicit lazy val regShort = new KryoRegistration[Short] { }

  implicit lazy val regByte = new KryoRegistration[Byte] { }
  
  implicit lazy val regProcessID = new KryoRegistration[ProcessID] {
    override def registerClassesWithSerializer = Seq(classOf[ProcessID] -> new ProcessIDSerializer)
  }
  
  implicit lazy val regTime = new KryoRegistration[Time] {
    override def registerClassesWithSerializer = Seq(classOf[Time] -> new TimeSerializer)
  }

  implicit lazy val regString = new KryoRegistration[String] {
    override def registerClasses = Seq(classOf[String])
  }

  implicit lazy val regByteArray= new KryoRegistration[Array[Byte]] {
    override def registerClasses = Seq(classOf[Array[Byte]])
  }

  implicit lazy val regBoolBoolPair = new KryoRegistration[(Boolean,Boolean)] {
    override def registerClasses = Seq(classOf[Tuple2[_,_]])
  }
  
  implicit lazy val regBoolIntPair = new KryoRegistration[(Boolean,Int)] {
    override def registerClasses = Seq(classOf[Tuple2[_,_]])
  }
  
  implicit lazy val regIntBoolPair = new KryoRegistration[(Int,Boolean)] {
    override def registerClasses = Seq(classOf[Tuple2[_,_]])
  }
  
  implicit lazy val regIntIntPair = new KryoRegistration[(Int,Int)] {
    override def registerClasses = Seq(classOf[Tuple2[_,_]])
  }
  
  implicit lazy val regStringIntPair = new KryoRegistration[(String,Int)] {
    override def registerClasses = Seq(classOf[String], classOf[Tuple2[_,_]])
  }
  
  implicit lazy val regIntTimePair = new KryoRegistration[(Int,Time)] {
    override def registerClasses = Seq(classOf[Tuple2[_,_]])
    override def registerClassesWithSerializer = Seq(classOf[Time] -> new TimeSerializer)
  }
  
  implicit lazy val regDoubleBoolPair = new KryoRegistration[(Double,Boolean)] {
    override def registerClasses = Seq(classOf[Tuple2[_,_]])
  }
  
  implicit lazy val regByteArrayTimePair = new KryoRegistration[(Array[Byte],Time)] {
    override def registerClasses = Seq(classOf[Array[Byte]], classOf[Tuple2[_,_]])
    override def registerClassesWithSerializer = Seq(classOf[Time] -> new TimeSerializer)
  }

  //TODO automatic conversion from Serializer[A] tp KryoRegistration[A]

}
