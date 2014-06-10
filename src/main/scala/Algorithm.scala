package round

abstract class Algorithm extends Variables
    with Processes
    with Rounds
    with Specs
{

  //round number
  val r = new GlobalVariable[Int](0)
  
  //the univers of processes
  val P = new Domain[Process]
  //and sets of Process
  val S = new Domain[Set[Process]]

  //number of processes
  val n = new LocalVariable[Int](0)

  //the heard-of set
  val HO = new LocalVariable[Set[Process]](Set[Process]())

  //
  val spec: Spec

  //
  def process(config: Map[String, Any]): Process

}
