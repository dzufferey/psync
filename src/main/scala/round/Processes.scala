package round

trait Processes[IO] {
  self: Algorithm[IO] =>

  abstract class Process{

    //use some Macro to rewrite that and to type the round correctly
    val rounds: List[Round[Any]]

    //to finish the instance
    protected def exit() {
      sys.error("TODO")
    }
  
    //////////////////
    // util methods //
    //////////////////

    //TODO the rounds ...

  }

}

