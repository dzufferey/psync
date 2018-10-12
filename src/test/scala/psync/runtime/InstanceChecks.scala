package psync.runtime

import org.scalacheck.{Properties, Prop, Gen}
import psync._


object InstanceChecks extends Properties("Instance# manipulation check") {

  property("id") = Prop forAll { (s: Short) => Instance.leq(s, s) && !Instance.lt(s, s) }

  property("leq") = Prop forAll { (p: (Long, Byte)) =>
    val delta = p._2
    val i1 = (p._1).toShort
    val i2 = (p._1 + delta).toShort
    if (delta >= 0)
      Instance.leq(i1, i2)
    else
      Instance.leq(i2, i1)
  }

  property("lt") = Prop forAll { (p: (Long, Byte)) =>
    val i1 = p._1
    val i2 = i1 + p._2
    if (p._2 > 0)
      Instance.lt(i1.toShort, i2.toShort)
    else if (p._2 < 0)
      Instance.lt(i2.toShort, i1.toShort)
    else
      !Instance.lt(i2.toShort, i1.toShort) && !Instance.lt(i1.toShort, i2.toShort)
  }
  
  property("simple catch up") = Prop forAll { (p: Long) =>
    Instance.catchUp(p, p.toShort) == p
  }

  property("catch up") = Prop forAll { (p: (Long, Byte)) =>
    val i1 = p._1
    val i2 = i1 + p._2
    Instance.catchUp(i1, i2.toShort) == i2
  }

}
