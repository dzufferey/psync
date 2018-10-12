package psync

//use a value class to keep ProcessID separated from short/int in the typechecker
class ProcessID(val id: Short) extends AnyVal

//IO is a type parameter to communicate the initial value, parameter, and callbacks
//the use of mixing composition forces elements (like variables) to be used only with the algorithm
abstract class Algorithm[IO, P <: Process[IO]] extends Specs[IO, P]
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

  def process: P

  /** A sample IO object. This is currently needed for the verification. */
  def dummyIO: IO

}

//placeholder for quantifying over some domain
class Domain[A] {
  def forall(fct: A => Boolean): Boolean = sys.error("only for specification purpose, removed by macros") // linter:ignore UnusedParameter
  def exists(fct: A => Boolean): Boolean = sys.error("only for specification purpose, removed by macros") // linter:ignore UnusedParameter
  def filter(fct: A => Boolean): Set[A] =  sys.error("only for specification purpose, removed by macros") // linter:ignore UnusedParameter
}

