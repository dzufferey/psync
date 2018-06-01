package psync.utils.serialization

import com.esotericsoftware.kryo.Serializer

/* register all the classes needed to (de)serialize type A */
trait KryoRegistration[A] {

  def registerClasses: Iterable[Class[_]] = Nil

  def registerClassesWithSerializer: Iterable[(Class[_],Serializer[_])] = Nil

}
  
