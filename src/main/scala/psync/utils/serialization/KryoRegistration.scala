package psync.utils.serialization

import com.esotericsoftware.kryo.Serializer
import com.esotericsoftware.kryo.Kryo

/* register all the classes needed to (de)serialize type A */
trait KryoRegistration[A] {

  def registerClasses: Iterable[Class[_]] = Nil

  def registerClassesWithSerializer: Iterable[(Class[_],Serializer[_])] = Nil

  def register(kryo: Kryo) = {
    registerClasses.foreach( kryo.register(_) )
    registerClassesWithSerializer.foreach{ case (c, s) => kryo.register(c, s) }
    kryo
  }

}
  
