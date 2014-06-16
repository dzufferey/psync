package round

//IO is a type parameter to communicate the initial value, parameter, and callbacks
abstract class Algorithm[IO] extends Variables[IO]
    with Processes[IO]
    with Rounds[IO]
    with Specs[IO]
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

  def process(id: Short, io: IO): Process

}
