package round.runtime

object Flags {
  final val normal = 0
  final val dummy = 1 //messages inserted to force progress
  final val error = 2
  //anything not used here may be used by the user
  //a message with a non-normal tag will be forwarded to handler rather than the normal flow of events
  final def userDefinable(flag: Byte) = flag < 0 || flag > 2
}

class Tag(val underlying: Long) extends AnyVal {

  //flag, code: a set of values that are partially used by our framework and userdefined

  //internal representation:
  // | flag | code | instance | round |
  //   1B      1B    2B         4B

  def instanceNbr: Short = {
    ((underlying >> 32) & 0xffffl).toShort
  }

  def setInstanceNbr(inst: Short): Tag = {
    val i = (inst.toLong & 0xffffl) << 32
    val u = underlying & 0xffff0000ffffffffl
    new Tag(u | i)
  }

  def roundNbr: Int = {
    underlying.toInt
  }

  def setRoundNbr(r: Int): Tag = {
    new Tag((underlying >> 32 << 32) | (r.toLong & 0xffffffffl))
  }

  def flag: Byte = {
    ((underlying >> 56) & 0xffl).toByte
  }

  def setFlag(flag: Byte): Tag = {
    val f = (flag.toLong & 0xffl) << 56
    val u = underlying & 0x00ffffffffffffffl
    new Tag(u | f)
  }

  def code: Byte = {
    ((underlying >> 48) & 0xffl).toByte
  }

  def setCode(code: Byte): Tag = {
    val c = (code.toLong & 0xffl) << 48
    val u = underlying & 0xff00ffffffffffffl
    new Tag(u | c)
  }

  override def toString = {
    "Tag("+ flag +","+ code +","+ instanceNbr +","+ roundNbr +")"
  }

}

object Tag {

  final val size = 8

  def apply(instance: Short, round: Int) = {
    new Tag(0).setInstanceNbr(instance).setRoundNbr(round)
  }

  def apply(instance: Short, round: Int, flag: Byte, code: Byte) = {
    new Tag(0).setFlag(flag).setCode(code).setInstanceNbr(instance).setRoundNbr(round)
  }

}
