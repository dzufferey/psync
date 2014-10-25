package round.runtime

/** instance number easily overflow and wrap around, here are some function to help deal with that */
object Instance {

  def lt(i1: Short, i2: Short): Boolean = {
    (i2 - i1).toShort > 0
  }

  def leq(i1: Short, i2: Short): Boolean = {
    (i2 - i1).toShort >= 0
  }

  def max(i1: Short, i2: Short): Short = {
    if (leq(i1, i2)) i2 else i1
  }

  def min(i1: Short, i2: Short): Short = {
    if (leq(i1, i2)) i1 else i2
  }

}
