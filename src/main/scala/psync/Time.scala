package psync

/** A dedicated type for time/round number.
 *  The comparison of time deal with the wrap around semantics of integer.
 *  It is correct as long as the difference between the two values is less than 2³¹-1.
 */
class Time(val toInt: Int) extends AnyVal with Ordered[Time] {
  def compare(that: Time) = this.toInt - that.toInt
  def tick = new Time(toInt + 1)
  def +(n : Int) = new Time(toInt + n)
  def -(n : Int) = new Time(toInt - n)
  def /(n : Int) = new Time(toInt / n) //to compute the phases from the round
}

object Time {
  implicit def fromInt(t: Int): Time = new Time(t)
  implicit def toInt(t: Time): Int = t.toInt
}

