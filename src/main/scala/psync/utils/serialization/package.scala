package psync.utils

import psync.{ProcessID, Time}
import scala.reflect.ClassTag
import com.esotericsoftware.kryo.Kryo

package object serialization {
  
  implicit lazy val regBool = new KryoRegistration[Boolean] { }

  implicit lazy val regInt = new KryoRegistration[Int] { }
  
  implicit lazy val regLong = new KryoRegistration[Long] { }

  implicit lazy val regShort = new KryoRegistration[Short] { }

  implicit lazy val regByte = new KryoRegistration[Byte] { }
  
  implicit lazy val regUnit = new KryoRegistration[Unit] {
    val s = new UnitSerializer
    override def registerClassesWithSerializer = Seq(
      classOf[Unit] -> s,
      classOf[scala.runtime.BoxedUnit] -> s
    )
  }
  
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

  implicit def regSet[A: ClassTag: KryoRegistration] = new KryoRegistration[Set[A]] {
    import scala.language.existentials // for Set.empty
    val setSerializer = new CollectionSerializer[A, Set[A]]
    override def registerClassesWithSerializer = Seq(
      classOf[Set[A]] -> setSerializer,
      classOf[scala.collection.immutable.Set.Set1[A]] -> setSerializer,
      classOf[scala.collection.immutable.Set.Set2[A]] -> setSerializer,
      classOf[scala.collection.immutable.Set.Set3[A]] -> setSerializer,
      classOf[scala.collection.immutable.Set.Set4[A]] -> setSerializer,
      classOf[scala.collection.immutable.HashSet[A]] -> setSerializer,
      classOf[scala.collection.immutable.HashSet.HashTrieSet[A]] -> setSerializer,
      Set.empty.getClass -> setSerializer
    )
    override def register(kryo: Kryo) = {
      implicitly[KryoRegistration[A]].register(kryo)
      super.register(kryo)
    }
  }

  //TODO automatic conversion from Serializer[A] tp KryoRegistration[A]

}
