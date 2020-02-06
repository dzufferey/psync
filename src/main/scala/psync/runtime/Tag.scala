package psync.runtime

import psync.Time

object Flags {
  final val normal = 0
  final val dummy = 1 //messages inserted to force progress
  final val error = 2
  //anything not used here may be used by the user
  //a message with a non-normal tag will be forwarded to handler rather than the normal flow of events
  final def userDefinable(flag: Byte) = flag < 0 || flag > 2
}

//in the end this will havea variable size and always be stored in the bytebuffer 
class Tag(val underlying: Long) extends AnyVal {

  //flag: a set of values that are partially used by our framework and userdefined
  //callStack: how many time value to expect (currently only 1)
  //instance: which instance this message belongs to
  //round: the round number

  //internal representation:
  // | flag | callStack | instance | round |
  //   1B     1B          2B         4B

  def instanceNbr: Short = {
    ((underlying >> 32) & 0xffffL).toShort
  }

  def setInstanceNbr(inst: Short): Tag = {
    val i = (inst.toLong & 0xffffL) << 32
    val u = underlying & 0xffff0000ffffffffL
    new Tag(u | i)
  }

  def roundNbr: Time = {
    new Time(underlying.toInt)
  }

  def setRoundNbr(r: Time): Tag = {
    new Tag((underlying >> 32 << 32) | (r.toInt.toLong & 0xffffffffL))
  }

  def flag: Byte = {
    ((underlying >> 56) & 0xffL).toByte
  }

  def setFlag(flag: Byte): Tag = {
    val f = (flag.toLong & 0xffL) << 56
    val u = underlying & 0x00ffffffffffffffL
    new Tag(u | f)
  }

  def callStack: Byte = {
    ((underlying >> 48) & 0xffL).toByte
  }

  def setCallStack(callStack: Byte): Tag = {
    val c = (callStack.toLong & 0xffL) << 48
    val u = underlying & 0xff00ffffffffffffL
    new Tag(u | c)
  }

  override def toString = {
    "Tag("+ flag +","+ callStack +","+ instanceNbr +","+ roundNbr +")"
  }

  def size = 8 //TODO 4 + callStack * 4

}

object Tag {

  def apply(instance: Short, round: Int) = {
    new Tag(0).setInstanceNbr(instance).setRoundNbr(round)
  }

  def apply(instance: Short, round: Int, flag: Byte, callStack: Byte) = {
    new Tag(0).setFlag(flag).setCallStack(callStack).setInstanceNbr(instance).setRoundNbr(round)
  }
  
  def roundNbr(tag: Tag): Time = new Time(tag.underlying.toInt)

  def flag(tag: Tag): Byte = ((tag.underlying >> 56) & 0xffL).toByte

  def code(tag: Tag): Byte = ((tag.underlying >> 48) & 0xffL).toByte

}
