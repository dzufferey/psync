package round

trait Processes[IO] {
  self: Algorithm[IO] =>

  abstract class Process(val id: Short) {
  }

}

