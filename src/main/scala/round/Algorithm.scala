package round

object Algorithm {
  type ProcessID = Short
}

import Algorithm._

//IO is a type parameter to communicate the initial value, parameter, and callbacks
//the use of mixing composition forces elements (like variables) to be used only with the algorithm
abstract class Algorithm[IO] extends Variables[IO]
    with Specs[IO]
{

  //round number
  val r = new GlobalVariable[Int](0)
  
  //the univers of processes
  val P = new Domain[ProcessID]
  //and sets of Process
  val S = new Domain[Set[ProcessID]]

  //number of processes
  val n = new LocalVariable[Int](0)

  //the heard-of set
  val HO = new LocalVariable[Set[ProcessID]](Set[ProcessID]())

  //specification of the consensus
  val spec: Spec

  def process(id: ProcessID, io: IO): Process


  //////////////////
  // util methods //
  //////////////////

}
