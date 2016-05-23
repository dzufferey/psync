package psync.runtime

/** instance number easily overflow and wrap around, here are some function to help deal with that
 *  this should get the right result if the two ids are separated by strictly less than |Short.MinValue|.
 */
object Instance {

  def compare(i1: Short, i2: Short): Int = {
    i1 - i2
  }

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

  /** catch-up: it is often the case that the # in the program get truncated to be used as a instance #.
   *  Here we try to recover the long # from the current # and an instance # */
  def catchUp(curr: Long, to: Short) = {
    curr + (to - curr.toShort).toShort
  }

}
