package round

//use a value class to keep ProcessID separated from short/int in the typechecker
class ProcessID(val id: Short) extends AnyVal

//IO is a type parameter to communicate the initial value, parameter, and callbacks
//the use of mixing composition forces elements (like variables) to be used only with the algorithm
abstract class Algorithm[IO, P <: Process[IO]] extends Specs[IO, P]
    //with Variables[IO]
{

  //round number
  var r = -1

  //number of processes
  var n = 0

  //the univers of processes
  val P = new Domain[P]
  //val P = new Domain[ProcessID]
  //and sets of Process
  val S = new Domain[Set[ProcessID]]

  //specification of the consensus
  val spec: Spec

  def process: P//rocess[IO]

  //////////////////
  // util methods //
  //////////////////

}

//the time/round number has its own type
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

//placeholder for quantifying over some domain
class Domain[A] {
  def forall(fct: A => Boolean): Boolean = sys.error("only for specification purpose, removed by macros")
  def exists(fct: A => Boolean): Boolean = sys.error("only for specification purpose, removed by macros")
  def filter(fct: A => Boolean): Set[A] =  sys.error("only for specification purpose, removed by macros")
}

