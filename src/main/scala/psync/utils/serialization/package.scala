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
  
  implicit lazy val regDouble = new KryoRegistration[Double] { }
  
  implicit lazy val regFloat = new KryoRegistration[Float] { }
  
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
    override def register(kryo: Kryo): Kryo = {
      val k1 = implicitly[KryoRegistration[A]].register(kryo)
      super.register(k1)
    }
  }
  
  implicit def regPair[A: ClassTag: KryoRegistration, B: ClassTag: KryoRegistration] = new KryoRegistration[(A,B)] {
    override def registerClasses = Seq(classOf[Tuple2[_,_]])
    override def register(kryo: Kryo): Kryo = {
      val k1 = implicitly[KryoRegistration[A]].register(kryo)
      val k2 = implicitly[KryoRegistration[B]].register(k1)
      super.register(k2)
    }
  }
  
  implicit def regOpt[A: ClassTag: KryoRegistration] = new KryoRegistration[Option[A]] {
    import scala.language.existentials // for None
    val optionSerializer = new OptionSerializer[A]
    override def registerClassesWithSerializer = Seq(
      classOf[Option[A]] -> optionSerializer,
      classOf[Some[A]] -> optionSerializer,
      None.getClass -> optionSerializer
    )
    override def register(kryo: Kryo): Kryo = {
      val k1 = implicitly[KryoRegistration[A]].register(kryo)
      super.register(k1)
    }
  }

  //TODO automatic conversion from Serializer[A] tp KryoRegistration[A]

}
