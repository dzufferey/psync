package psync.utils.serialization

object KryoSerializer {

  import com.twitter.chill._
  private val inst = new EmptyScalaKryoInstantiator

  def serializer = {
    val kryo = inst.newKryo
    kryo.setRegistrationRequired(true)
    kryo
  }

}


